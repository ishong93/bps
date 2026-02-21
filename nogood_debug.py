"""
nogood_debug.py — ATMS Nogood 생애 주기 Mock 디버깅 시뮬레이션
================================================================

new-nogood의 5단계를 실제 데이터로 단계별 추적합니다:
  ① env.nogood? 표시 (금지 도장)
  ② remove-env-from-labels (모든 label에서 제거)
  ③ nogood-table에 등록
  ④ 기존 상위 nogood 정리 (최소성)
  ⑤ env-table 내 상위집합 오염 (상향 전파)

Mock 시나리오:
  Mock 1: 기본 nogood 등록과 label 제거
  Mock 2: 상향 오염 — nogood {A,B}가 {A,B,C}를 오염
  Mock 3: weave에서의 nogood 차단과 우회 경로 발견
  Mock 4: 다중 nogood과 최소성 정리

원본 코드 참조: atms.lisp:367-406
"""

from __future__ import annotations
from typing import Optional, List, Dict, Set, Any, FrozenSet
from dataclasses import dataclass, field
import copy


# ══════════════════════════════════════════════════════════════════
# 1. ATMS 데이터 타입 시뮬레이션
# ══════════════════════════════════════════════════════════════════

class Env:
    """환경(Environment) — 가정들의 집합으로 표현되는 하나의 가능한 세계."""
    _counter = 0

    def __init__(self, assumptions: FrozenSet[str]):
        Env._counter += 1
        self.index = Env._counter
        self.assumptions = assumptions
        self.count = len(assumptions)
        self.nogood: Any = None        # None=유효, 그 외=nogood 이유
        self.nodes: List[Node] = []    # 이 env를 label에 가진 노드들 (역참조)

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


class Node:
    """TMS 노드 — 하나의 명제."""
    def __init__(self, name: str, contradictory: bool = False):
        self.name = name
        self.label: List[Env] = []       # minimal env들의 antichain
        self.contradictory = contradictory
        self.assumption = False

    def __repr__(self) -> str:
        return self.name

    def label_str(self) -> str:
        if not self.label:
            return "[]"
        return "[" + ", ".join(str(e) for e in self.label) + "]"


class ATMS:
    """ATMS 엔진 — env-table, nogood-table, contra-node를 관리."""
    def __init__(self, title: str):
        self.title = title
        self.env_table: List[Env] = []
        self.nogood_table: List[Env] = []
        self.nodes: List[Node] = []
        self.contra_node = Node("⊥-CONTRADICTION", contradictory=True)
        # 빈 환경 생성
        self.empty_env = self._create_env_raw(frozenset())

    def _create_env_raw(self, assumptions: FrozenSet[str]) -> Env:
        """env 생성 (nogood 검사 없이)."""
        env = Env(assumptions)
        self.env_table.append(env)
        return env

    def create_env(self, assumptions: FrozenSet[str]) -> Env:
        """env 생성 후 즉시 nogood 여부 검사."""
        # 기존 env 재사용
        for e in self.env_table:
            if e.assumptions == assumptions:
                return e
        env = Env(assumptions)
        self.env_table.append(env)
        # set-env-contradictory: 기존 nogood의 부분집합인지 검사
        self._set_env_contradictory(env)
        return env

    def create_node(self, name: str, assumption: bool = False) -> Node:
        node = Node(name)
        self.nodes.append(node)
        if assumption:
            node.assumption = True
            env = self.create_env(frozenset({name}))
            node.label.append(env)
            env.nodes.append(node)
        return node

    def _set_env_contradictory(self, env: Env) -> bool:
        """
        새 env가 기존 nogood의 상위집합인지 검사.
        원본: atms.lisp:387-397
        """
        if env.is_nogood():
            return True
        for ng in self.nogood_table:
            if ng.assumptions <= env.assumptions:  # ng ⊆ env
                env.nogood = f"⊇{ng}"
                return True
        return False

    # ══════════════════════════════════════════════════════════════
    # new-nogood: 핵심 5단계
    # 원본: atms.lisp:367-385
    # ══════════════════════════════════════════════════════════════

    def new_nogood(self, env: Env, reason: str, trace: bool = True) -> None:
        """nogood 등록의 5단계를 추적 가능하게 실행."""
        if trace:
            print(f"\n  ┌─ new-nogood 시작: {env}")
            print(f"  │  이유: {reason}")

        # ── 단계 ① 금지 도장 ──────────────────────────────────
        env.nogood = reason
        if trace:
            print(f"  │")
            print(f"  │  ① 금지 도장: {env}.nogood = '{reason}'")

        # ── 단계 ② label에서 제거 ─────────────────────────────
        removed_from = self._remove_env_from_labels(env, trace)

        # ── 단계 ③ nogood-table에 등록 ────────────────────────
        self.nogood_table.append(env)
        if trace:
            print(f"  │")
            print(f"  │  ③ nogood-table 등록: {self._nogood_table_str()}")

        # ── 단계 ④ 기존 상위 nogood 정리 (최소성) ─────────────
        removed_nogoods = []
        for ng in list(self.nogood_table):
            if ng is not env and ng.count > env.count:
                if env.assumptions <= ng.assumptions:  # env ⊂ ng
                    removed_nogoods.append(ng)
                    self.nogood_table.remove(ng)
        if trace:
            print(f"  │")
            if removed_nogoods:
                print(f"  │  ④ 상위 nogood 정리: {removed_nogoods} 제거 (최소성)")
                print(f"  │    정리 후 nogood-table: {self._nogood_table_str()}")
            else:
                print(f"  │  ④ 상위 nogood 정리: 해당 없음")

        # ── 단계 ⑤ env-table 내 상위집합 오염 ─────────────────
        contaminated = []
        for e in self.env_table:
            if e is not env and not e.is_nogood() and e.count > env.count:
                if env.assumptions <= e.assumptions:  # env ⊆ e
                    e.nogood = f"⊇{env}"
                    self._remove_env_from_labels(e, trace=False)
                    contaminated.append(e)
        if trace:
            print(f"  │")
            if contaminated:
                print(f"  │  ⑤ 상향 오염: {contaminated}")
                for c in contaminated:
                    print(f"  │    {c} — 오염됨 (∵ {env} ⊆ {c})")
            else:
                print(f"  │  ⑤ 상향 오염: 해당 없음")

            print(f"  └─ new-nogood 완료")

    def _remove_env_from_labels(self, env: Env, trace: bool = True) -> List[str]:
        """
        이 env를 참조하는 모든 노드의 label에서 제거.
        원본: atms.lisp:399-406
        """
        removed_from = []
        for node in list(env.nodes):
            if env in node.label:
                node.label.remove(env)
                removed_from.append(node.name)
        env.nodes.clear()

        if trace:
            print(f"  │")
            if removed_from:
                print(f"  │  ② label에서 제거:")
                for name in removed_from:
                    n = next(n for n in self.nodes if n.name == name)
                    print(f"  │    {name}.label → {n.label_str()}")
            else:
                print(f"  │  ② label에서 제거: 해당 노드 없음")
        return removed_from

    # ══════════════════════════════════════════════════════════════
    # union-env: nogood 차단 포함
    # 원본: atms.lisp:307-314
    # ══════════════════════════════════════════════════════════════

    def union_env(self, e1: Env, e2: Env, trace: bool = True) -> Optional[Env]:
        """두 env를 합집합으로 병합. nogood이면 None 반환."""
        merged_assumptions = e1.assumptions | e2.assumptions
        result = self.create_env(merged_assumptions)

        if trace:
            if result.is_nogood():
                print(f"      union-env({e1}, {e2}) → {result} ⚠️ NOGOOD! → None")
                return None
            else:
                print(f"      union-env({e1}, {e2}) → {result} ✓")

        if result.is_nogood():
            return None
        return result

    # ══════════════════════════════════════════════════════════════
    # weave: 교차곱 + nogood 필터링 + 최소성
    # 원본: atms.lisp:245-263
    # ══════════════════════════════════════════════════════════════

    def weave(self, antecedent: Node, envs: List[Env],
              antecedents: List[Node], trace: bool = True) -> Optional[List[Env]]:
        """교차곱 계산. nogood 필터링 + 최소성 유지."""
        current_envs = list(envs)

        if trace:
            print(f"\n  ┌─ weave 시작")
            print(f"  │  antecedent={antecedent} (건너뜀)")
            print(f"  │  시작 envs={current_envs}")
            print(f"  │  antecedents={antecedents}")

        for node in antecedents:
            if node is antecedent:
                if trace:
                    print(f"  │")
                    print(f"  │  ── {node} 건너뜀 (자기 자신)")
                continue

            if trace:
                print(f"  │")
                print(f"  │  ── {node} 처리 시작 (label={node.label_str()})")

            new_envs: List[Env] = []
            for env in current_envs:
                for node_env in node.label:
                    if trace:
                        print(f"  │    {env} × {node_env}:", end="")

                    merged = self.union_env(env, node_env, trace=False)

                    if merged is None or merged.is_nogood():
                        if trace:
                            reason = "nogood" if merged and merged.is_nogood() else "union 실패"
                            print(f" → {merged if merged else '?'} ⚠️ {reason} → 차단!")
                        continue

                    # antichain 삽입 (최소성 검사)
                    insert_result = self._insert_minimal(merged, new_envs, trace)
                    if trace:
                        if insert_result == "DOMINATED":
                            print(f" → {merged} 지배당함(DOMINATED) → 버림")
                        elif insert_result == "DOMINATES":
                            print(f" → {merged} 삽입 + 기존 지배항목 제거")
                        else:
                            print(f" → {merged} ✓ 삽입")

            current_envs = new_envs
            if trace:
                print(f"  │    결과: {current_envs}")

            if not current_envs:
                if trace:
                    print(f"  │    ⚡ 조기 종료! (유효한 env 없음)")
                    print(f"  └─ weave 결과: None")
                return None

        if trace:
            print(f"  │")
            print(f"  └─ weave 결과: {current_envs}")
        return current_envs

    def _insert_minimal(self, new_env: Env, acc: List[Env],
                        trace: bool = True) -> str:
        """antichain에 삽입. 최소성 유지."""
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

        if to_remove:
            return "DOMINATES"
        return "INSERTED"

    def _compare_env(self, e1: Env, e2: Env) -> Optional[str]:
        a1, a2 = e1.assumptions, e2.assumptions
        if a1 == a2: return 'EQ'
        if a1 < a2:  return 'S12'
        if a2 < a1:  return 'S21'
        return None

    # ══════════════════════════════════════════════════════════════
    # update: contra-node 도달 시 nogood 생성
    # 원본: atms.lisp:206-224
    # ══════════════════════════════════════════════════════════════

    def update(self, new_envs: List[Env], consequence: Node,
               reason: str, trace: bool = True) -> Optional[List[Env]]:
        """consequence가 contradictory이면 nogood 생성."""
        if trace:
            print(f"\n  ┌─ update 시작")
            print(f"  │  new_envs={new_envs}")
            print(f"  │  consequence={consequence}")
            print(f"  │  contradictory?={consequence.contradictory}")

        if consequence.contradictory:
            if trace:
                print(f"  │  ⚠️ 결론이 모순 노드! → 각 env를 nogood으로 등록")
                print(f"  └─ update: nogood 생성 모드")
            for env in new_envs:
                self.new_nogood(env, reason, trace)
            return None

        # 일반 노드: label 갱신
        added = self._update_label(consequence, new_envs, trace)
        if trace:
            print(f"  │  label 갱신 결과: {consequence.label_str()}")
            print(f"  └─ update 완료")
        return added

    def _update_label(self, node: Node, new_envs: List[Env],
                      trace: bool = True) -> List[Env]:
        """노드 label에 new_envs를 최소성 유지하며 추가."""
        added = []
        for new_env in new_envs:
            dominated = False
            to_remove = []
            for existing in node.label:
                rel = self._compare_env(new_env, existing)
                if rel in ('EQ', 'S21'):
                    dominated = True
                    break
                elif rel == 'S12':
                    to_remove.append(existing)

            if not dominated:
                for r in to_remove:
                    node.label.remove(r)
                    if node in r.nodes:
                        r.nodes.remove(node)
                node.label.append(new_env)
                new_env.nodes.append(node)
                added.append(new_env)

        return added

    # ══════════════════════════════════════════════════════════════
    # 유틸리티
    # ══════════════════════════════════════════════════════════════

    def _nogood_table_str(self) -> str:
        if not self.nogood_table:
            return "[]"
        return "[" + ", ".join(str(e) for e in self.nogood_table) + "]"

    def print_state(self, title: str = "") -> None:
        if title:
            print(f"\n  ═══ {title} ═══")

        print(f"\n  env-table:")
        for e in self.env_table:
            status = " ✗ NOGOOD" if e.is_nogood() else " ✓"
            refs = ", ".join(n.name for n in e.nodes) if e.nodes else "-"
            print(f"    {str(e):<30s}{status}  (참조: {refs})")

        print(f"\n  nogood-table: {self._nogood_table_str()}")

        print(f"\n  노드 labels:")
        for n in self.nodes:
            status = " [CONTRA]" if n.contradictory else ""
            label = n.label_str()
            if n.assumption:
                print(f"    {n.name:<12s}(가정){status}  label={label}")
            else:
                print(f"    {n.name:<12s}      {status}  label={label}")


# ══════════════════════════════════════════════════════════════════
# MOCK 테스트
# ══════════════════════════════════════════════════════════════════

def mock1():
    """
    Mock 1: 기본 nogood 등록과 label 제거
    ─────────────────────────────────────
    시나리오:
      가정 A, B, C
      추론: A+B → D  (A와 B가 동시에 참이면 D가 참)
      D.label에 E{A,B}가 등록된 상태에서
      nogood-nodes로 {A,B}를 모순 선언

    기대:
      - E{A,B}가 nogood으로 표시
      - D.label에서 E{A,B}가 제거되어 빈 label
      - 이후 weave에서 {A,B} 조합이 차단
    """
    Env.reset_counter()
    print("=" * 70)
    print("  Mock 1: 기본 nogood 등록과 label 제거")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B, C
    추론 J1: A + B → D
    이후 nogood 선언: {A, B}는 모순!

  질문: D의 label은 어떻게 되는가?
  """)

    atms = ATMS("Mock1")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    D = atms.create_node("D")

    # J1: A + B → D  (weave로 교차곱 계산)
    print("  ── 단계 1: justify-node J1: A + B → D")
    new_envs = atms.weave(None, [atms.empty_env], [A, B], trace=True)

    if new_envs:
        atms.update(new_envs, D, "J1", trace=True)

    atms.print_state("J1 적용 후 상태 (nogood 전)")

    # nogood 선언: {A, B} → 모순!
    print("\n" + "─" * 70)
    print("  ── 단계 2: nogood-nodes({A, B}) 실행!")
    print("  ── 이것은 내부적으로 justify-node('NOGOOD', contra-node, [A, B])과 동일")
    print("  ── weave로 교차곱 → E{A,B} → update(contra-node) → new-nogood")

    # nogood-nodes의 내부 동작을 시뮬레이션
    nogood_envs = atms.weave(None, [atms.empty_env], [A, B], trace=True)
    if nogood_envs:
        atms.update(nogood_envs, atms.contra_node, "NOGOOD:A+B", trace=True)

    atms.print_state("nogood {A,B} 등록 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    e_ab = next(e for e in atms.env_table if e.assumptions == frozenset({"A", "B"}))
    assert e_ab.is_nogood(), "E{A,B}는 nogood이어야 함"
    print(f"    ✓ E{{A,B}} is nogood: {e_ab.is_nogood()}")
    assert D.label == [], "D.label은 비어야 함 (E{A,B}가 제거됨)"
    print(f"    ✓ D.label은 비어있음: {D.label_str()}")
    print(f"    ✓ A.label은 유효: {A.label_str()}")
    print(f"    ✓ B.label은 유효: {B.label_str()}")


def mock2():
    """
    Mock 2: 상향 오염 — nogood {A,B}가 {A,B,C}를 오염
    ──────────────────────────────────────────────────
    시나리오:
      가정 A, B, C
      추론: A+B → D,  A+B+C → E (D+C → E)
      E.label에 E{A,B,C}가 등록된 상태에서
      nogood {A,B}가 발생

    기대:
      - E{A,B,C}가 자동으로 오염 (∵ {A,B} ⊆ {A,B,C})
      - E.label에서도 제거
      - D.label, E.label 모두 비어짐
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 2: 상향 오염 — nogood {A,B}가 {A,B,C}를 자동 오염")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B, C
    추론 J1: A + B → D
    추론 J2: D + C → E  (즉, A+B+C 세계에서 E가 참)
    이후 nogood 선언: {A, B}는 모순!

  질문: E{A,B,C}는 어떻게 되는가?
         (E의 label에 있던 E{A,B,C}는?)
  """)

    atms = ATMS("Mock2")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    D = atms.create_node("D")
    E = atms.create_node("E")

    # J1: A + B → D
    print("  ── 단계 1: J1: A + B → D")
    envs1 = atms.weave(None, [atms.empty_env], [A, B], trace=True)
    if envs1:
        atms.update(envs1, D, "J1", trace=True)

    # J2: D + C → E
    print("\n  ── 단계 2: J2: D + C → E")
    envs2 = atms.weave(None, D.label, [D, C], trace=True)
    if envs2:
        atms.update(envs2, E, "J2", trace=True)

    atms.print_state("J1, J2 적용 후 상태 (nogood 전)")

    # nogood 선언
    print("\n" + "─" * 70)
    print("  ── 단계 3: nogood({A, B}) 발생!")
    print("  ── 핵심: {A,B} ⊆ {A,B,C} 이므로 E{A,B,C}도 오염되어야 함")

    e_ab = next(e for e in atms.env_table if e.assumptions == frozenset({"A", "B"}))
    atms.new_nogood(e_ab, "NOGOOD:A+B", trace=True)

    atms.print_state("nogood {A,B} 등록 후 최종 상태")

    # 검증
    print("\n  ── 검증:")
    e_abc = next((e for e in atms.env_table
                  if e.assumptions == frozenset({"A", "B", "C"})), None)
    if e_abc:
        assert e_abc.is_nogood(), "E{A,B,C}도 nogood이어야 함 (상향 오염)"
        print(f"    ✓ E{{A,B,C}} is nogood: {e_abc.is_nogood()} (상향 오염!)")
    assert D.label == [], "D.label은 비어야 함"
    print(f"    ✓ D.label 비어있음: {D.label_str()}")
    assert E.label == [], "E.label은 비어야 함 (E{A,B,C}도 오염)"
    print(f"    ✓ E.label 비어있음: {E.label_str()}")
    print(f"    ✓ A.label 유효: {A.label_str()}")
    print(f"    ✓ B.label 유효: {B.label_str()}")
    print(f"    ✓ C.label 유효: {C.label_str()}")


def mock3():
    """
    Mock 3: weave에서의 nogood 차단과 우회 경로 발견
    ──────────────────────────────────────────────────
    시나리오 (atest.lisp:step-1을 재현):
      가정 A, B, C
      J1: A → x=1
      J2: B → y=x
      J3: C → x=z
      nogood({A, B})
      J4: x=1 + y=x → y=1   ← A+B 경로가 차단됨!
      Premise: z=1 (무조건 참)
      J5: z=1 + x=z → x=1   ← x=1에 새 경로 추가!
      J4 재시도: x=1(C경유) + y=x → y=1  ← C+B 우회 경로 발견!

    기대:
      - {A,B} 경유 y=1 불가
      - {B,C} 경유 y=1 가능!
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 3: nogood 차단과 우회 경로 발견 (step-1 재현)")
    print("=" * 70)
    print("""
  시나리오 (atest.lisp step-1):
    가정 A, B, C
    J1: A → x=1      J2: B → y=x      J3: C → x=z
    nogood({A, B})
    J4: x=1 + y=x → y=1    ← {A,B} 차단!
    Premise: z=1             ← 무조건 참
    J5: z=1 + x=z → x=1    ← x=1에 {C} 경로 추가!
    J4 재시도                 ← {B,C} 우회 경로 발견!
  """)

    atms = ATMS("Mock3-step1")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    x1 = atms.create_node("x=1")
    yx = atms.create_node("y=x")
    xz = atms.create_node("x=z")
    y1 = atms.create_node("y=1")
    z1 = atms.create_node("z=1")

    # J1: A → x=1
    print("  ── J1: A → x=1")
    envs = atms.weave(None, [atms.empty_env], [A], trace=False)
    if envs:
        atms.update(envs, x1, "J1", trace=False)
    print(f"    x=1.label = {x1.label_str()}")

    # J2: B → y=x
    print("  ── J2: B → y=x")
    envs = atms.weave(None, [atms.empty_env], [B], trace=False)
    if envs:
        atms.update(envs, yx, "J2", trace=False)
    print(f"    y=x.label = {yx.label_str()}")

    # J3: C → x=z
    print("  ── J3: C → x=z")
    envs = atms.weave(None, [atms.empty_env], [C], trace=False)
    if envs:
        atms.update(envs, xz, "J3", trace=False)
    print(f"    x=z.label = {xz.label_str()}")

    atms.print_state("초기 추론 완료")

    # nogood 등록
    print("\n" + "─" * 70)
    print("  ── nogood({A, B}) 선언!")
    e_ab = atms.create_env(frozenset({"A", "B"}))
    atms.new_nogood(e_ab, "NOGOOD:A+B", trace=True)

    # J4: x=1 + y=x → y=1  (A+B 경로 → 차단!)
    print("\n" + "─" * 70)
    print("  ── J4: x=1 + y=x → y=1  (A+B 경로를 시도)")
    print(f"    x=1.label = {x1.label_str()}")
    print(f"    y=x.label = {yx.label_str()}")
    envs = atms.weave(None, x1.label, [x1, yx], trace=True)
    if envs:
        atms.update(envs, y1, "J4", trace=True)
    else:
        print("\n    ⚡ weave가 None 반환! — {A}×{B} = {A,B}가 nogood이므로 차단!")
    print(f"    y=1.label = {y1.label_str()}  ← 아직 비어있음!")

    # Premise: z=1 (무조건 참)
    print("\n" + "─" * 70)
    print("  ── Premise: z=1 (무조건 참 — 빈 env로 지지)")
    z1.label.append(atms.empty_env)
    atms.empty_env.nodes.append(z1)
    print(f"    z=1.label = {z1.label_str()}")

    # J5: z=1 + x=z → x=1  (C 경유 새 경로!)
    print("\n" + "─" * 70)
    print("  ── J5: z=1 + x=z → x=1  (C 경유 새 경로!)")
    print(f"    z=1.label = {z1.label_str()}")
    print(f"    x=z.label = {xz.label_str()}")
    envs = atms.weave(None, z1.label, [z1, xz], trace=True)
    if envs:
        atms.update(envs, x1, "J5", trace=True)
    print(f"    x=1.label = {x1.label_str()}  ← E{{A}}와 E{{C}} 두 경로!")

    # J4 재시도: x=1(E{C}) + y=x(E{B}) → y=1(E{B,C})
    print("\n" + "─" * 70)
    print("  ── J4 재전파: x=1 + y=x → y=1  (이번엔 E{C} 경로 사용!)")
    print(f"    x=1.label = {x1.label_str()}")
    print(f"    y=x.label = {yx.label_str()}")
    envs = atms.weave(None, x1.label, [x1, yx], trace=True)
    if envs:
        atms.update(envs, y1, "J4-retry", trace=True)
    print(f"    y=1.label = {y1.label_str()}")

    atms.print_state("최종 상태")

    # 검증
    print("\n  ── 검증:")
    assert len(y1.label) == 1, "y=1에는 정확히 1개의 env"
    y1_env = y1.label[0]
    assert y1_env.assumptions == frozenset({"B", "C"}), "y=1은 {B,C} 세계"
    print(f"    ✓ y=1.label = {y1.label_str()} — {{B,C}} 우회 경로 발견!")
    print(f"    ✓ E{{A,B}} 경로는 nogood으로 차단됨")
    print(f"    ✓ E{{B,C}} 경로로 y=1이 증명됨 — ATMS의 진가!")


def mock4():
    """
    Mock 4: 다중 nogood과 최소성 정리
    ──────────────────────────────────
    시나리오:
      가정 A, B, C, D
      먼저 큰 nogood {A,B,C}를 등록
      이후 작은 nogood {A,B}를 등록
      → {A,B,C}가 nogood-table에서 제거 (최소성)

      추가로:
      {C,D}도 nogood으로 등록
      → 두 독립 nogood이 동시에 작동

      weave: A × B × C × D 교차곱에서
      → {A,B}, {C,D} 모두 차단되는지 확인
    """
    Env.reset_counter()
    print("\n\n" + "=" * 70)
    print("  Mock 4: 다중 nogood과 최소성 정리")
    print("=" * 70)
    print("""
  시나리오:
    가정 A, B, C, D
    1단계: nogood({A,B,C}) 등록 — "큰" 금지
    2단계: nogood({A,B}) 등록   — "작은" 금지 (더 강력!)
    기대: {A,B,C}가 nogood-table에서 제거 (최소성: {A,B}가 이미 커버)

    3단계: nogood({C,D}) 추가 등록 — 독립적 금지
    4단계: weave로 다양한 조합 시도
    기대: {A,B}와 {C,D} 모두 차단
  """)

    atms = ATMS("Mock4")
    A = atms.create_node("A", assumption=True)
    B = atms.create_node("B", assumption=True)
    C = atms.create_node("C", assumption=True)
    D = atms.create_node("D", assumption=True)
    P = atms.create_node("P")
    Q = atms.create_node("Q")

    # P = A+B+C 세계에서 참, Q = A+C+D 세계에서 참
    e_abc = atms.create_env(frozenset({"A", "B", "C"}))
    P.label.append(e_abc); e_abc.nodes.append(P)
    e_acd = atms.create_env(frozenset({"A", "C", "D"}))
    Q.label.append(e_acd); e_acd.nodes.append(Q)

    atms.print_state("초기 상태")

    # 1단계: 큰 nogood {A,B,C}
    print("\n" + "─" * 70)
    print("  ── 1단계: nogood({A,B,C}) 등록")
    atms.new_nogood(e_abc, "BIG-NOGOOD", trace=True)
    atms.print_state("nogood {A,B,C} 등록 후")

    # 검증 1
    print("\n  ── 검증 1:")
    assert e_abc.is_nogood(), "E{A,B,C}는 nogood"
    print(f"    ✓ E{{A,B,C}} is nogood")
    assert P.label == [], "P.label은 비어야 함"
    print(f"    ✓ P.label 비어있음: {P.label_str()}")
    assert len(atms.nogood_table) == 1
    print(f"    ✓ nogood-table 크기: 1 ({atms._nogood_table_str()})")

    # 2단계: 작은 nogood {A,B}  → 최소성 정리!
    print("\n" + "─" * 70)
    print("  ── 2단계: nogood({A,B}) 등록 — 더 강력한 금지!")
    print("  ── 핵심: {A,B} ⊂ {A,B,C} 이므로 {A,B,C}는 불필요해짐")
    e_ab = atms.create_env(frozenset({"A", "B"}))
    atms.new_nogood(e_ab, "SMALL-NOGOOD", trace=True)
    atms.print_state("nogood {A,B} 등록 후")

    # 검증 2
    print("\n  ── 검증 2:")
    assert e_ab.is_nogood(), "E{A,B}는 nogood"
    print(f"    ✓ E{{A,B}} is nogood")
    # {A,B,C}는 nogood-table에서 제거 (최소성) — 하지만 env 자체는 여전히 nogood
    in_table = [e for e in atms.nogood_table if e.assumptions == frozenset({"A", "B", "C"})]
    assert len(in_table) == 0, "{A,B,C}는 nogood-table에서 제거되어야 함 (최소성)"
    print(f"    ✓ {{A,B,C}}는 nogood-table에서 제거됨 (최소성: {atms._nogood_table_str()})")
    print(f"    ✓ 하지만 E{{A,B,C}}.is_nogood()는 여전히 True: {e_abc.is_nogood()}")

    # 3단계: 독립 nogood {C,D}
    print("\n" + "─" * 70)
    print("  ── 3단계: nogood({C,D}) 등록 — 독립적 금지")
    e_cd = atms.create_env(frozenset({"C", "D"}))
    atms.new_nogood(e_cd, "ANOTHER-NOGOOD", trace=True)

    # E{A,C,D}도 오염 확인
    atms.print_state("nogood {C,D} 등록 후")

    # 검증 3
    print("\n  ── 검증 3:")
    assert e_cd.is_nogood()
    print(f"    ✓ E{{C,D}} is nogood")
    assert e_acd.is_nogood(), "E{A,C,D}도 오염 (∵ {C,D} ⊆ {A,C,D})"
    print(f"    ✓ E{{A,C,D}} is nogood: {e_acd.is_nogood()} (상향 오염!)")
    assert Q.label == [], "Q.label은 비어야 함"
    print(f"    ✓ Q.label 비어있음: {Q.label_str()}")
    print(f"    ✓ nogood-table: {atms._nogood_table_str()}")

    # 4단계: weave로 다양한 조합 시도
    print("\n" + "─" * 70)
    print("  ── 4단계: weave 테스트 — 다중 nogood이 동시에 차단하는지 확인")

    print("\n  [4a] A×B 시도 (→ {A,B} 차단):")
    result_ab = atms.weave(None, A.label, [A, B], trace=True)
    assert result_ab is None
    print(f"    ✓ 결과: None — {'{A,B}'} nogood에 의해 차단!")

    print("\n  [4b] C×D 시도 (→ {C,D} 차단):")
    result_cd = atms.weave(None, C.label, [C, D], trace=True)
    assert result_cd is None
    print(f"    ✓ 결과: None — {'{C,D}'} nogood에 의해 차단!")

    print("\n  [4c] A×C 시도 (→ {A,C} 유효!):")
    result_ac = atms.weave(None, A.label, [A, C], trace=True)
    assert result_ac is not None
    print(f"    ✓ 결과: {result_ac} — 유효! (어떤 nogood에도 해당 안 함)")

    print("\n  [4d] B×D 시도 (→ {B,D} 유효!):")
    result_bd = atms.weave(None, B.label, [B, D], trace=True)
    assert result_bd is not None
    print(f"    ✓ 결과: {result_bd} — 유효!")


# ══════════════════════════════════════════════════════════════════
# 실행
# ══════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    mock1()
    mock2()
    mock3()
    mock4()

    print("\n\n" + "=" * 70)
    print("  🏁 모든 Mock 테스트 통과!")
    print("=" * 70)
    print("""
  요약:
    Mock 1: 기본 nogood 등록 → label에서 제거 확인
    Mock 2: 상향 오염 — {A,B} nogood → {A,B,C}도 자동 오염
    Mock 3: nogood 차단 후 우회 경로 발견 (step-1 재현)
    Mock 4: 다중 nogood + 최소성 정리 + 동시 차단
  """)
