"""
justify_node_debug.py — justify-node 함수의 전체 파이프라인 Mock 디버깅
====================================================================

justify-node의 내부 동작을 단계별로 추적합니다:

  ① Justification 구조체 생성 (informant, consequence, antecedents)
  ② 양방향 링크 등록 (consequence.justs ← just, antecedent.consequences ← just)
  ③ propagate(just, nil, [E∅]) 호출
     └─ weave(nil, [E∅], antecedents) → 교차곱
     └─ update(new_envs, consequence, just)
        ├─ 일반 노드: update-label → label 갱신 → 하류 재전파
        └─ 모순 노드: new-nogood 생성

Mock 시나리오:
  Mock 1: 기본 파이프라인 — just 생성 → propagate → weave → update
  Mock 2: 연쇄 전파 — 새 justification이 하류를 자동 갱신
  Mock 3: 다중 Justification + Premise — 같은 노드에 여러 근거
  Mock 4: nogood-nodes — justify-node으로 모순 선언하는 변형

원본 코드 참조: atms.lisp:178-224
"""

from __future__ import annotations
from typing import Optional, List, Any, FrozenSet
from dataclasses import dataclass, field


# ══════════════════════════════════════════════════════════════════
# 1. ATMS 데이터 타입
# ══════════════════════════════════════════════════════════════════

class Env:
    """환경(Environment) — 가정들의 집합으로 표현되는 하나의 가능한 세계."""
    _counter = 0

    def __init__(self, assumptions: FrozenSet[str]):
        Env._counter += 1
        self.index = Env._counter
        self.assumptions = assumptions
        self.count = len(assumptions)
        self.nogood: Any = None
        self.nodes: List[Node] = []

    def is_nogood(self) -> bool:
        return self.nogood is not None

    def __repr__(self) -> str:
        content = "+".join(sorted(self.assumptions)) if self.assumptions else "∅"
        mark = f"✗({self.nogood})" if self.is_nogood() else ""
        return f"E-{self.index}{{{content}}}{mark}"

    def __eq__(self, other) -> bool:
        return isinstance(other, Env) and self.assumptions == other.assumptions

    def __hash__(self) -> int:
        return hash(self.assumptions)

    @staticmethod
    def reset_counter():
        Env._counter = 0


class Just:
    """Justification — 추론 규칙 하나를 표현하는 구조체.
    원본: atms.lisp:55-59
    """
    def __init__(self, index: int, informant: str,
                 consequence: 'Node', antecedents: List['Node']):
        self.index = index
        self.informant = informant
        self.consequence = consequence
        self.antecedents = antecedents

    def __repr__(self) -> str:
        antes = ", ".join(n.name for n in self.antecedents)
        return f"<{self.informant} J{self.index}: [{antes}] → {self.consequence.name}>"


class Node:
    """TMS 노드 — 하나의 명제."""
    def __init__(self, name: str, contradictory: bool = False):
        self.name = name
        self.label: List[Env] = []
        self.justs: List[Just] = []         # 이 노드를 결론으로 하는 justification들
        self.consequences: List[Just] = []  # 이 노드가 전제로 쓰인 justification들
        self.contradictory = contradictory
        self.assumption = False

    def __repr__(self) -> str:
        return self.name

    def label_str(self) -> str:
        if not self.label:
            return "[]"
        return "[" + ", ".join(str(e) for e in self.label) + "]"


class ATMS:
    """ATMS 엔진."""
    def __init__(self, title: str):
        self.title = title
        self.node_counter = 0
        self.just_counter = 0
        self.env_table: List[Env] = []
        self.nogood_table: List[Env] = []
        self.nodes: List[Node] = []
        self.justs: List[Just] = []
        self.contra_node = Node("⊥", contradictory=True)
        self.empty_env = self._create_env_raw(frozenset())

    def _create_env_raw(self, assumptions: FrozenSet[str]) -> Env:
        env = Env(assumptions)
        self.env_table.append(env)
        return env

    def create_env(self, assumptions: FrozenSet[str]) -> Env:
        for e in self.env_table:
            if e.assumptions == assumptions:
                return e
        env = Env(assumptions)
        self.env_table.append(env)
        self._set_env_contradictory(env)
        return env

    def create_node(self, name: str, assumption: bool = False) -> Node:
        self.node_counter += 1
        node = Node(name)
        self.nodes.append(node)
        if assumption:
            node.assumption = True
            env = self.create_env(frozenset({name}))
            node.label.append(env)
            env.nodes.append(node)
        return node

    def _set_env_contradictory(self, env: Env) -> bool:
        if env.is_nogood():
            return True
        for ng in self.nogood_table:
            if ng.assumptions <= env.assumptions:
                env.nogood = f"⊇{ng}"
                return True
        return False

    # ══════════════════════════════════════════════════════════════
    # justify-node: 핵심 함수 — 추론 규칙을 ATMS에 등록
    # 원본: atms.lisp:178-193
    # ══════════════════════════════════════════════════════════════

    def justify_node(self, informant: str, consequence: Node,
                     antecedents: List[Node], trace: bool = True) -> Just:
        """
        justify-node의 전체 파이프라인:
          ① Justification 구조체 생성
          ② 양방향 링크 등록
          ③ propagate(just, nil, [E∅]) 호출
        """
        # ── ① 구조체 생성 ──────────────────────────────────────
        self.just_counter += 1
        just = Just(
            index=self.just_counter,
            informant=informant,
            consequence=consequence,
            antecedents=antecedents
        )

        if trace:
            print(f"\n  ┌─ justify-node 시작")
            print(f"  │")
            print(f"  │  ① Justification 구조체 생성:")
            print(f"  │     {just}")
            print(f"  │     index     = {just.index}")
            print(f"  │     informant = '{just.informant}'")
            print(f"  │     consequence  = {just.consequence.name}")
            antes_str = ", ".join(n.name for n in antecedents) if antecedents else "(없음)"
            print(f"  │     antecedents = [{antes_str}]")

        # ── ② 양방향 링크 등록 ─────────────────────────────────
        consequence.justs.append(just)
        for node in antecedents:
            node.consequences.append(just)
        self.justs.append(just)

        if trace:
            print(f"  │")
            print(f"  │  ② 양방향 링크 등록:")
            print(f"  │     {consequence.name}.justs ← {just}")
            for node in antecedents:
                print(f"  │     {node.name}.consequences ← {just}")
            print(f"  │     atms.justs에 등록 (총 {len(self.justs)}개)")

        # ── ③ propagate 호출 ───────────────────────────────────
        if trace:
            print(f"  │")
            print(f"  │  ③ propagate(just, antecedent=None, envs=[E∅]) 호출")
            print(f"  │     → weave에서 모든 antecedent를 처리 (건너뛸 것 없음)")
            print(f"  └─ justify-node → propagate로 진입")

        self._propagate(just, antecedent=None,
                        envs=[self.empty_env], trace=trace, depth=1)

        return just

    # ══════════════════════════════════════════════════════════════
    # propagate: weave → update 파이프라인
    # 원본: atms.lisp:202-204
    # ══════════════════════════════════════════════════════════════

    def _propagate(self, just: Just, antecedent: Optional[Node],
                   envs: List[Env], trace: bool = True, depth: int = 1) -> None:
        indent = "  " + "│  " * (depth - 1)

        if trace:
            source = antecedent.name if antecedent else "None"
            print(f"\n{indent}┌─ propagate (depth={depth})")
            print(f"{indent}│  just       = {just}")
            print(f"{indent}│  antecedent = {source}")
            print(f"{indent}│  envs       = {envs}")
            print(f"{indent}│")
            print(f"{indent}│  → weave({source}, envs, [{', '.join(n.name for n in just.antecedents)}])")

        new_envs = self._weave(antecedent, envs, just.antecedents,
                               trace=trace, depth=depth)

        if new_envs is None:
            if trace:
                print(f"{indent}│  ⚡ weave → None (nogood 차단)")
                print(f"{indent}│  → update 호출 생략!")
                print(f"{indent}└─ propagate 종료 (전파 중단)")
            return

        if trace:
            print(f"{indent}│")
            print(f"{indent}│  → update({new_envs}, {just.consequence.name})")

        self._update(new_envs, just.consequence, just,
                     trace=trace, depth=depth)

        if trace:
            print(f"{indent}└─ propagate 종료")

    # ══════════════════════════════════════════════════════════════
    # weave: 교차곱 + nogood 필터링 + 최소성
    # 원본: atms.lisp:245-263
    # ══════════════════════════════════════════════════════════════

    def _weave(self, antecedent: Optional[Node], envs: List[Env],
               antecedents: List[Node],
               trace: bool = True, depth: int = 1) -> Optional[List[Env]]:
        indent = "  " + "│  " * (depth - 1)
        current_envs = list(envs)

        if trace:
            print(f"{indent}│")
            print(f"{indent}│  ┌─ weave 시작")
            ante_name = antecedent.name if antecedent else "None"
            print(f"{indent}│  │  antecedent={ante_name} (건너뜀)")
            print(f"{indent}│  │  시작 envs={current_envs}")

        for node in antecedents:
            if node is antecedent:
                if trace:
                    print(f"{indent}│  │")
                    print(f"{indent}│  │  ── {node} 건너뜀 (antecedent=자기 자신)")
                    print(f"{indent}│  │     ↳ envs가 이미 이 노드의 새 env를 포함하므로 스킵!")
                continue

            if trace:
                print(f"{indent}│  │")
                print(f"{indent}│  │  ── {node} 처리 (label={node.label_str()})")

            new_envs: List[Env] = []
            for env in current_envs:
                for node_env in node.label:
                    merged = self._union_env(env, node_env)

                    if trace:
                        print(f"{indent}│  │    {env} × {node_env}:", end="")

                    if merged is None or merged.is_nogood():
                        if trace:
                            print(f" → ⚠️ nogood! 차단")
                        continue

                    # antichain 삽입
                    result = self._insert_minimal(merged, new_envs)
                    if trace:
                        if result == "DOMINATED":
                            print(f" → {merged} 지배당함 → 버림")
                        elif result == "DOMINATES":
                            print(f" → {merged} 삽입 + 기존 항목 대체")
                        else:
                            print(f" → {merged} ✓ 삽입")

            current_envs = new_envs
            if trace:
                print(f"{indent}│  │    결과: {current_envs}")

            if not current_envs:
                if trace:
                    print(f"{indent}│  │    ⚡ 빈 결과 → 조기 종료!")
                    print(f"{indent}│  └─ weave → None")
                return None

        if trace:
            print(f"{indent}│  │")
            print(f"{indent}│  └─ weave → {current_envs}")
        return current_envs

    # ══════════════════════════════════════════════════════════════
    # update: label 갱신 + 모순 처리 + 하류 재전파
    # 원본: atms.lisp:206-224
    # ══════════════════════════════════════════════════════════════

    def _update(self, new_envs: List[Env], consequence: Node,
                just: Just, trace: bool = True, depth: int = 1) -> None:
        indent = "  " + "│  " * (depth - 1)

        if trace:
            print(f"{indent}│")
            print(f"{indent}│  ┌─ update 시작")
            print(f"{indent}│  │  new_envs    = {new_envs}")
            print(f"{indent}│  │  consequence = {consequence.name}")
            print(f"{indent}│  │  contradictory? = {consequence.contradictory}")

        # ── 모순 노드 경우: nogood 생성 ────────────────────────
        if consequence.contradictory:
            if trace:
                print(f"{indent}│  │  ⚠️ consequence가 모순 노드(⊥)!")
                print(f"{indent}│  │  → 각 env를 nogood으로 등록")
            for env in new_envs:
                self._new_nogood(env, just.informant, trace=trace, depth=depth)
            if trace:
                print(f"{indent}│  └─ update 종료 (nogood 생성 완료)")
            return

        # ── 일반 노드: label 갱신 ──────────────────────────────
        if trace:
            print(f"{indent}│  │")
            print(f"{indent}│  │  일반 노드 → update-label 호출")
            old_label = consequence.label_str()

        added = self._update_label(consequence, new_envs, trace=trace, depth=depth)

        if trace:
            print(f"{indent}│  │  label 변화: {old_label} → {consequence.label_str()}")
            if added:
                print(f"{indent}│  │  실제 추가된 env: {added}")
            else:
                print(f"{indent}│  │  추가된 env 없음 (이미 지배당함)")

        if not added:
            if trace:
                print(f"{indent}│  └─ update 종료 (변화 없음, 전파 중단)")
            return

        # ── 하류 재전파 ────────────────────────────────────────
        downstream = consequence.consequences
        if trace:
            if downstream:
                print(f"{indent}│  │")
                print(f"{indent}│  │  하류 justification {len(downstream)}개 재전파:")
                for dj in downstream:
                    print(f"{indent}│  │    {dj}")
            else:
                print(f"{indent}│  │  하류 justification 없음 (전파 종료)")

        if trace:
            print(f"{indent}│  └─ update 완료")

        for supported_just in downstream:
            if trace:
                print(f"\n{indent}│  ▶ 재전파: {supported_just}")
                print(f"{indent}│    antecedent={consequence.name}, envs={added}")
                print(f"{indent}│    → weave에서 {consequence.name}를 건너뛰고 나머지만 처리!")

            self._propagate(supported_just, consequence, added,
                            trace=trace, depth=depth + 1)

    # ══════════════════════════════════════════════════════════════
    # update-label: 최소성 유지하며 label 갱신
    # 원본: atms.lisp:226-243
    # ══════════════════════════════════════════════════════════════

    def _update_label(self, node: Node, new_envs: List[Env],
                      trace: bool = True, depth: int = 1) -> List[Env]:
        indent = "  " + "│  " * (depth - 1)
        added = []

        for new_env in new_envs:
            dominated = False
            to_remove = []

            for existing in node.label:
                rel = self._compare_env(new_env, existing)
                if rel in ('EQ', 'S21'):
                    dominated = True
                    if trace:
                        reason = "동일" if rel == 'EQ' else f"{existing} ⊂ {new_env}"
                        print(f"{indent}│  │    {new_env} vs {existing}: "
                              f"{rel} → 지배당함 ({reason})")
                    break
                elif rel == 'S12':
                    to_remove.append(existing)
                    if trace:
                        print(f"{indent}│  │    {new_env} vs {existing}: "
                              f"S12 → {new_env} ⊂ {existing}, 기존 제거!")

            if not dominated:
                for r in to_remove:
                    node.label.remove(r)
                    if node in r.nodes:
                        r.nodes.remove(node)
                node.label.append(new_env)
                new_env.nodes.append(node)
                added.append(new_env)
                if trace and not to_remove:
                    print(f"{indent}│  │    {new_env}: 새로 추가 ✓")

        return added

    # ══════════════════════════════════════════════════════════════
    # new-nogood: 5단계 처리 (간략)
    # 원본: atms.lisp:367-385
    # ══════════════════════════════════════════════════════════════

    def _new_nogood(self, env: Env, reason: str,
                    trace: bool = True, depth: int = 1) -> None:
        indent = "  " + "│  " * (depth - 1)

        if trace:
            print(f"{indent}│  │")
            print(f"{indent}│  │  ┌─ new-nogood: {env}")
            print(f"{indent}│  │  │  이유: {reason}")

        # ① 금지 표시
        env.nogood = reason

        # ② label에서 제거
        removed_from = []
        for node in list(env.nodes):
            if env in node.label:
                node.label.remove(env)
                removed_from.append(node.name)
        env.nodes.clear()

        # ③ nogood-table 등록
        self.nogood_table.append(env)

        # ④ 상위 nogood 정리
        removed_nogoods = []
        for ng in list(self.nogood_table):
            if ng is not env and ng.count > env.count:
                if env.assumptions <= ng.assumptions:
                    removed_nogoods.append(ng)
                    self.nogood_table.remove(ng)

        # ⑤ 상향 오염
        contaminated = []
        for e in self.env_table:
            if e is not env and not e.is_nogood() and e.count > env.count:
                if env.assumptions <= e.assumptions:
                    e.nogood = f"⊇{env}"
                    for node in list(e.nodes):
                        if e in node.label:
                            node.label.remove(e)
                    e.nodes.clear()
                    contaminated.append(e)

        if trace:
            print(f"{indent}│  │  │  ① 금지 도장: {env}")
            if removed_from:
                print(f"{indent}│  │  │  ② label 제거: {removed_from}")
            print(f"{indent}│  │  │  ③ nogood-table: [{', '.join(str(e) for e in self.nogood_table)}]")
            if removed_nogoods:
                print(f"{indent}│  │  │  ④ 상위 정리: {removed_nogoods} 제거")
            if contaminated:
                print(f"{indent}│  │  │  ⑤ 상향 오염: {contaminated}")
            print(f"{indent}│  │  └─ new-nogood 완료")

    # ══════════════════════════════════════════════════════════════
    # nogood-nodes: justify-node의 변형
    # 원본: atms.lisp:195-198
    # ══════════════════════════════════════════════════════════════

    def nogood_nodes(self, informant: str, nodes: List[Node],
                     trace: bool = True) -> Just:
        """모순 선언. 내부적으로 justify-node(informant, ⊥, nodes)."""
        if trace:
            names = ", ".join(n.name for n in nodes)
            print(f"\n  ══ nogood-nodes('{informant}', [{names}])")
            print(f"     ↳ justify-node('{informant}', ⊥-contra-node, [{names}])")
            print(f"     ↳ contra-node.contradictory = True 이므로")
            print(f"        update에서 new-nogood가 호출됨!")
        return self.justify_node(informant, self.contra_node, nodes, trace=trace)

    # ══════════════════════════════════════════════════════════════
    # 헬퍼 함수들
    # ══════════════════════════════════════════════════════════════

    def _union_env(self, e1: Env, e2: Env) -> Optional[Env]:
        merged = e1.assumptions | e2.assumptions
        return self.create_env(merged)

    def _insert_minimal(self, new_env: Env, acc: List[Env]) -> str:
        to_remove = []
        for existing in acc:
            rel = self._compare_env(new_env, existing)
            if rel in ('EQ', 'S21'):
                return "DOMINATED"
            elif rel == 'S12':
                to_remove.append(existing)
        for r in to_remove:
            acc.remove(r)
        acc.append(new_env)
        return "DOMINATES" if to_remove else "INSERTED"

    def _compare_env(self, e1: Env, e2: Env) -> Optional[str]:
        a1, a2 = e1.assumptions, e2.assumptions
        if a1 == a2: return 'EQ'
        if a1 < a2:  return 'S12'
        if a2 < a1:  return 'S21'
        return None

    def print_state(self, title: str = "") -> None:
        if title:
            print(f"\n  ═══ {title} ═══")

        print(f"\n  env-table:")
        for e in self.env_table:
            status = " ✗ NOGOOD" if e.is_nogood() else " ✓"
            refs = ", ".join(n.name for n in e.nodes) if e.nodes else "-"
            print(f"    {str(e):<35s}{status}  (참조: {refs})")

        print(f"\n  justification 목록:")
        if self.justs:
            for j in self.justs:
                print(f"    {j}")
        else:
            print(f"    (없음)")

        print(f"\n  노드 상태:")
        for n in self.nodes:
            tag = " (가정)" if n.assumption else ""
            label = n.label_str()
            justs_info = ", ".join(j.informant for j in n.justs) if n.justs else "-"
            downstream = ", ".join(j.informant for j in n.consequences) if n.consequences else "-"
            print(f"    {n.name:<10s}{tag:<8s} label={label:<25s} "
                  f"justs=[{justs_info}]  downstream=[{downstream}]")


# ══════════════════════════════════════════════════════════════════
# MOCK 테스트
# ══════════════════════════════════════════════════════════════════

def mock1():
    """
    Mock 1: 기본 justify-node 파이프라인
    ─────────────────────────────────────
    시나리오:
      가정 A, B
      justify-node("J1", D, [A, B])

    추적 포인트:
      ① Just 구조체 생성: index, informant, consequence, antecedents
      ② 양방향 링크: D.justs ← J1, A.consequences ← J1, B.consequences ← J1
      ③ propagate → weave([E∅], [A,B]) → E{A,B} → update(D)
    """
    Env.reset_counter()
    print("=" * 70)
    print("  Mock 1: 기본 justify-node 파이프라인")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B
    justify-node("J1", D, [A, B])

  추적할 흐름:
    justify-node
      ① Just 구조체 생성
      ② 양방향 링크 등록
      ③ propagate(J1, None, [E∅])
          └─ weave(None, [E∅], [A, B])
              └─ E∅ × E{A} = E{A}
              └─ E{A} × E{B} = E{A,B}
          └─ update([E{A,B}], D)
              └─ update-label: D.label = [E{A,B}]
    """)

    atms = ATMS("Mock1")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    D = atms.create_node("D")

    print("  ── 초기 상태:")
    print(f"    A.label = {A.label_str()}")
    print(f"    B.label = {B.label_str()}")
    print(f"    D.label = {D.label_str()}")

    print("\n" + "─" * 70)
    print("  ── justify-node('J1', D, [A, B]) 호출!")
    just = atms.justify_node("J1", D, [A, B], trace=True)

    atms.print_state("justify-node 완료 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    assert len(D.label) == 1
    assert D.label[0].assumptions == frozenset({"A", "B"})
    print(f"    ✓ D.label = {D.label_str()} — A+B 세계에서 D가 참")

    assert just in D.justs
    print(f"    ✓ D.justs에 {just.informant} 등록됨")

    assert just in A.consequences
    assert just in B.consequences
    print(f"    ✓ A.consequences에 J1 등록됨 → A가 바뀌면 J1이 재전파됨")
    print(f"    ✓ B.consequences에 J1 등록됨 → B가 바뀌면 J1이 재전파됨")


def mock2():
    """
    Mock 2: 연쇄 전파 — justify-node가 하류를 자동 갱신
    ────────────────────────────────────────────────────
    시나리오:
      가정 A, B, C
      ① justify-node("J1", D, [A, B]) → D.label = [E{A,B}]
      ② justify-node("J2", E, [D, C]) → E.label = [E{A,B,C}]
      ③ justify-node("J3", D, [C])    → D.label에 E{C} 추가!
         → 핵심: J2가 자동 재전파! E.label이 [E{C}]로 갱신!

    추적 포인트:
      - J3 적용 시 D.label = [E{A,B}, E{C}]
      - D.consequences에 J2가 있으므로 자동 재전파
      - propagate(J2, D, [E{C}]):
        weave에서 D를 건너뛰고 C만 처리
        E{C} × E{C} = E{C}
      - update-label: E.label에 E{C} 추가
        E{C} ⊂ E{A,B,C} → E{A,B,C} 제거 → E.label = [E{C}]!
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 2: 연쇄 전파 — justify-node가 하류를 자동 갱신")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B, C
    ① justify-node("J1", D, [A, B])  → D.label = [E{A,B}]
    ② justify-node("J2", E, [D, C])  → E.label = [E{A,B,C}]
    ③ justify-node("J3", D, [C])     → D에 E{C} 추가!
       → J2 자동 재전파! E.label이 어떻게 변하는가?

  핵심 질문:
    E{C} ⊂ E{A,B,C} 이므로 E{A,B,C}가 제거되어
    E.label = [E{C}] 가 되어야 한다!
    (= "E는 C만 있으면 참"이 되는 것)
    """)

    atms = ATMS("Mock2")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    D = atms.create_node("D")
    E = atms.create_node("E")

    # ── ① J1: A + B → D ──
    print("  ── ① justify-node('J1', D, [A, B])")
    atms.justify_node("J1", D, [A, B], trace=True)
    print(f"\n    결과: D.label = {D.label_str()}")

    # ── ② J2: D + C → E ──
    print("\n" + "─" * 70)
    print("  ── ② justify-node('J2', E, [D, C])")
    print("       D.consequences에 J2가 등록됨 → D가 바뀌면 J2 재전파!")
    atms.justify_node("J2", E, [D, C], trace=True)
    print(f"\n    결과: E.label = {E.label_str()}")

    print(f"\n    ⚡ 지금 D.consequences = [{', '.join(j.informant for j in D.consequences)}]")
    print(f"    → D.label이 바뀌면 J2가 자동 재전파된다!")

    # ── ③ J3: C → D (핵심!) ──
    print("\n" + "═" * 70)
    print("  ── ③ justify-node('J3', D, [C])")
    print("       ★ D에 새 경로 추가 → J2가 재전파되어 E.label이 변화!")
    print("═" * 70)
    atms.justify_node("J3", D, [C], trace=True)

    atms.print_state("J3 적용 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    assert len(D.label) == 2
    d_envs = {e.assumptions for e in D.label}
    assert frozenset({"A", "B"}) in d_envs
    assert frozenset({"C"}) in d_envs
    print(f"    ✓ D.label = {D.label_str()} — 두 경로: {{A,B}}와 {{C}}")

    assert len(E.label) == 1
    assert E.label[0].assumptions == frozenset({"C"})
    print(f"    ✓ E.label = {E.label_str()} — {{C}} 하나만 남음!")
    print(f"      ↳ E{{C}} ⊂ E{{A,B,C}} 이므로 E{{A,B,C}} 제거됨 (최소성)")
    print(f"      ↳ 의미: 'C만 참이면 E도 참' (더 강력한 근거)")

    print(f"\n    ★ 재전파 메커니즘:")
    print(f"      D.label에 E{{C}} 추가 → D.consequences에 J2 있음")
    print(f"      → propagate(J2, D, [E{{C}}])")
    print(f"      → weave에서 D 건너뜀 (antecedent), C만 처리")
    print(f"      → E{{C}} × E{{C}} = E{{C}} → update(E)")
    print(f"      → E{{C}} ⊂ E{{A,B,C}} → E{{A,B,C}} 제거!")


def mock3():
    """
    Mock 3: Premise + 다중 Justification
    ─────────────────────────────────────
    시나리오:
      justify-node("PREMISE", P, [])  → P.label = [E{∅}] (무조건 참!)
      가정 A, B
      justify-node("J1", D, [A])    → D.label = [E{A}]
      justify-node("J2", D, [B])    → D.label = [E{A}, E{B}]
      justify-node("J3", E, [D, P]) → E.label = [E{A}, E{B}]

    추적 포인트:
      - Premise: 빈 antecedents → weave가 시작 envs를 그대로 반환
      - 다중 Justification: D에 J1, J2 두 근거 → label에 두 env
      - P.label = [E∅]이므로 E∅ × E{A} = E{A}, E∅ × E{B} = E{B}
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 3: Premise + 다중 Justification")
    print("=" * 70)
    print("""
  시나리오:
    ① justify-node("PREMISE", P, [])  → antecedents 없음 = 무조건 참
    ② justify-node("J1", D, [A])
    ③ justify-node("J2", D, [B])      → D에 두 번째 근거!
    ④ justify-node("J3", E, [D, P])   → D와 P의 교차곱

  핵심 질문:
    - Premise의 label은?
    - D에 두 개의 justification이 있으면 label은?
    - E∅(Premise) × E{A,B}(D의 label) = ?
    """)

    atms = ATMS("Mock3")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    P = atms.create_node("P")
    D = atms.create_node("D")
    E = atms.create_node("E")

    # ── ① Premise: P (빈 antecedents) ──
    print("  ── ① justify-node('PREMISE', P, [])  ← antecedents가 비어있음!")
    print("       weave에서 처리할 antecedent가 없으므로 시작 envs=[E∅]가 그대로 반환")
    atms.justify_node("PREMISE", P, [], trace=True)
    print(f"\n    결과: P.label = {P.label_str()}")
    print(f"    ↳ E{{∅}} = '아무 가정 없이도 참' = 무조건 참(Premise)!")

    # ── ② J1: A → D ──
    print("\n" + "─" * 70)
    print("  ── ② justify-node('J1', D, [A])")
    atms.justify_node("J1", D, [A], trace=True)
    print(f"\n    결과: D.label = {D.label_str()}")

    # ── ③ J2: B → D (같은 노드에 두 번째 justification!) ──
    print("\n" + "─" * 70)
    print("  ── ③ justify-node('J2', D, [B])  ← D에 두 번째 근거!")
    print("       D.label에 이미 E{A}가 있지만 E{B}는 비교불가(incomparable)")
    print("       → 둘 다 유지!")
    atms.justify_node("J2", D, [B], trace=True)
    print(f"\n    결과: D.label = {D.label_str()}")
    print(f"    ↳ 두 근거: J1(A→D)과 J2(B→D)가 독립적으로 D를 지지")

    # ── ④ J3: D + P → E ──
    print("\n" + "─" * 70)
    print("  ── ④ justify-node('J3', E, [D, P])  ← D(label 2개) × P(E∅)")
    print("       weave가 D의 label [E{A}, E{B}]와 P의 label [E{∅}]을 교차곱:")
    print("         E{A} × E{∅} = E{A}  ✓")
    print("         E{B} × E{∅} = E{B}  ✓")
    atms.justify_node("J3", E, [D, P], trace=True)

    atms.print_state("모든 justification 적용 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    assert len(P.label) == 1
    assert P.label[0].assumptions == frozenset()
    print(f"    ✓ P.label = {P.label_str()} — Premise는 빈 env (무조건 참)")

    assert len(D.label) == 2
    d_envs = {e.assumptions for e in D.label}
    assert frozenset({"A"}) in d_envs
    assert frozenset({"B"}) in d_envs
    print(f"    ✓ D.label = {D.label_str()} — 두 독립 근거")

    assert len(E.label) == 2
    e_envs = {e.assumptions for e in E.label}
    assert frozenset({"A"}) in e_envs
    assert frozenset({"B"}) in e_envs
    print(f"    ✓ E.label = {E.label_str()} — P가 E∅이므로 D의 label 그대로 계승!")
    print(f"      ↳ Premise(E∅)는 교차곱에서 '투명' — 다른 env를 변경하지 않음")


def mock4():
    """
    Mock 4: nogood-nodes — justify-node의 모순 선언 변형
    ─────────────────────────────────────────────────────
    시나리오:
      가정 A, B, C
      J1: A + B → D
      J2: D + C → E
      nogood-nodes("CONFLICT", [A, B])
        = justify-node("CONFLICT", ⊥, [A, B])

    추적 포인트:
      - nogood-nodes는 justify-node에 contra-node를 넘길 뿐
      - weave 결과 E{A,B} → update(⊥) → contradictory=True → new-nogood!
      - new-nogood 5단계로 상향 오염
      - D.label, E.label 모두 비워짐
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 4: nogood-nodes — justify-node의 모순 선언 변형")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B, C
    ① J1: A + B → D
    ② J2: D + C → E       → E.label = [E{A,B,C}]
    ③ nogood-nodes("CONFLICT", [A, B])
       = justify-node("CONFLICT", ⊥-contra-node, [A, B])

  핵심:
    justify-node에 contra-node(contradictory=True)를 넘기면
    update에서 new-nogood가 호출됨!
    → E{A,B} nogood → D.label 비움 → E{A,B,C} 오염 → E.label 비움
    """)

    atms = ATMS("Mock4")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    D = atms.create_node("D")
    E = atms.create_node("E")

    # ① J1: A + B → D
    print("  ── ① justify-node('J1', D, [A, B])")
    atms.justify_node("J1", D, [A, B], trace=False)
    print(f"    D.label = {D.label_str()}")

    # ② J2: D + C → E
    print("\n  ── ② justify-node('J2', E, [D, C])")
    atms.justify_node("J2", E, [D, C], trace=False)
    print(f"    E.label = {E.label_str()}")

    atms.print_state("nogood 전 상태")

    # ③ nogood-nodes
    print("\n" + "═" * 70)
    print("  ── ③ nogood-nodes('CONFLICT', [A, B]) 호출!")
    print("═" * 70)
    atms.nogood_nodes("CONFLICT", [A, B], trace=True)

    atms.print_state("nogood-nodes 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    e_ab = next(e for e in atms.env_table if e.assumptions == frozenset({"A", "B"}))
    assert e_ab.is_nogood()
    print(f"    ✓ E{{A,B}} is nogood: {e_ab.is_nogood()}")

    e_abc = next((e for e in atms.env_table
                  if e.assumptions == frozenset({"A", "B", "C"})), None)
    if e_abc:
        assert e_abc.is_nogood()
        print(f"    ✓ E{{A,B,C}} is nogood: {e_abc.is_nogood()} (상향 오염)")

    assert D.label == []
    print(f"    ✓ D.label = {D.label_str()} — 비워짐")
    assert E.label == []
    print(f"    ✓ E.label = {E.label_str()} — 비워짐 (상향 오염)")
    print(f"    ✓ A.label = {A.label_str()} — 유효 (개별 가정은 무사)")
    print(f"    ✓ B.label = {B.label_str()} — 유효")

    print(f"\n    ★ nogood-nodes의 정체:")
    print(f"      nogood-nodes(info, [A,B])")
    print(f"        = justify-node(info, ⊥-contra-node, [A,B])")
    print(f"        → propagate → weave → E{{A,B}}")
    print(f"        → update(⊥): contradictory=True!")
    print(f"        → new-nogood(E{{A,B}}) 호출!")
    print(f"      '모순을 증명하는 것'이 곧 'nogood을 만드는 것'")


# ══════════════════════════════════════════════════════════════════
# 실행
# ══════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    mock1()
    mock2()
    mock3()
    mock4()

    print("\n\n" + "=" * 70)
    print("  모든 Mock 테스트 통과!")
    print("=" * 70)
    print("""
  요약:

    Mock 1: 기본 파이프라인
      justify-node → ①구조체 생성 → ②양방향 링크 → ③propagate
      propagate → weave(교차곱) → update(label 갱신)

    Mock 2: 연쇄 전파 (★ justify-node의 핵심 가치)
      D에 새 justification 추가 → D.consequences의 J2가 자동 재전파
      → E.label이 [E{A,B,C}] → [E{C}]로 최소화!
      "한 곳의 변화가 전체 네트워크에 자동 파급"

    Mock 3: Premise + 다중 Justification
      빈 antecedents → label = [E∅] (무조건 참)
      같은 노드에 여러 justification → label에 여러 env 공존
      Premise(E∅)는 교차곱에서 투명 (다른 env를 변경하지 않음)

    Mock 4: nogood-nodes = justify-node의 변형
      justify-node(info, ⊥-contra-node, nodes)
      → update에서 contradictory=True 감지
      → new-nogood 호출 → 5단계 처리
      "모순을 증명하는 것 = nogood을 만드는 것"
    """)
