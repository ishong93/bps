"""
weave 함수 단일 케이스 디버깅 트레이스
======================================

입력 조건:
  정당화: P ∧ Q → 결론
  P.label = [{A, 1}, {B}]
  Q.label = [{A, 2}, {B, 1}]
  antecedent = nil
  envs = [{}]

교차곱 후보 (P × Q = 4쌍):
  {A,1} × {A,2} = {A,1,2}
  {A,1} × {B,1} = {A,B,1}
  {B}   × {A,2} = {A,B,2}
  {B}   × {B,1} = {B,1}

최소성 필터:
  {B,1} ⊂ {A,B,1} → {A,B,1} 제거
  결과: [{A,1,2}, {A,B,2}, {B,1}]

원본 코드 참조: atms.lisp:245-263
"""

from __future__ import annotations
from typing import Optional, List, Any, FrozenSet


# ══════════════════════════════════════════════════════════════════
# 데이터 타입
# ══════════════════════════════════════════════════════════════════

class Env:
    _counter = 0

    def __init__(self, assumptions: FrozenSet[str]):
        Env._counter += 1
        self.index = Env._counter
        self.assumptions = assumptions
        self.count = len(assumptions)
        self.nogood: Any = None

    def is_nogood(self) -> bool:
        return self.nogood is not None

    def __repr__(self) -> str:
        content = "+".join(sorted(self.assumptions)) if self.assumptions else "∅"
        return f"E-{self.index}{{{content}}}"

    def __eq__(self, other) -> bool:
        return isinstance(other, Env) and self.assumptions == other.assumptions

    def __hash__(self) -> int:
        return hash(self.assumptions)

    @staticmethod
    def reset():
        Env._counter = 0


class Node:
    def __init__(self, name: str):
        self.name = name
        self.label: List[Env] = []

    def __repr__(self) -> str:
        return self.name

    def label_str(self) -> str:
        return "[" + ", ".join(str(e) for e in self.label) + "]"


# ══════════════════════════════════════════════════════════════════
# env 캐시
# ══════════════════════════════════════════════════════════════════

env_cache: dict[FrozenSet[str], Env] = {}

def get_env(assumptions: FrozenSet[str]) -> Env:
    if assumptions not in env_cache:
        env_cache[assumptions] = Env(assumptions)
    return env_cache[assumptions]


# ══════════════════════════════════════════════════════════════════
# compare-env (원본: atms.lisp:356-363)
# ══════════════════════════════════════════════════════════════════

def compare_env(e1: Env, e2: Env) -> Optional[str]:
    """
    :EQ  — 동일
    :S12 — e1 ⊂ e2 (e1이 더 작음, e1이 e2를 지배)
    :S21 — e2 ⊂ e1 (e2가 더 작음, e2가 e1을 지배)
    None — 비교불가 (incomparable)
    """
    a1, a2 = e1.assumptions, e2.assumptions
    if a1 == a2: return 'EQ'
    if a1 < a2:  return 'S12'   # a1 ⊂ a2 (proper subset)
    if a2 < a1:  return 'S21'   # a2 ⊂ a1
    return None                  # incomparable


# ══════════════════════════════════════════════════════════════════
# weave (원본: atms.lisp:245-263) — 단계별 추적 버전
# ══════════════════════════════════════════════════════════════════

def weave_trace(antecedent, envs: List[Env], antecedents: List[Node]):
    """
    weave의 원본 알고리즘을 한 줄 한 줄 추적합니다.

    알고리즘 요약:
      for each node in antecedents (antecedent 자신은 건너뜀):
        new_envs = []
        for each env in current_envs:
          for each node_env in node.label:
            new_env = union(env, node_env)
            if not nogood:
              antichain 삽입 (최소성 유지)
        current_envs = new_envs
        if empty → return nil
      return current_envs
    """
    current_envs = list(envs)

    print(f"  ┌─ weave 시작")
    ante_name = antecedent.name if antecedent else "None"
    print(f"  │  antecedent = {ante_name} (이 노드는 건너뜀)")
    print(f"  │  시작 envs  = {current_envs}")
    print(f"  │  antecedents = {[n.name for n in antecedents]}")

    step = 0
    for node in antecedents:
        if node is antecedent:
            print(f"  │")
            print(f"  │  ── {node.name}: 건너뜀 (antecedent = 자기 자신)")
            continue

        step += 1
        print(f"  │")
        print(f"  │  {'═' * 55}")
        print(f"  │  단계 {step}: {node.name} 처리")
        print(f"  │  {node.name}.label = {node.label_str()}")
        print(f"  │  현재 envs = {current_envs}")
        print(f"  │  ── 교차곱: current_envs × {node.name}.label ──")

        new_envs: List[Env] = []
        pair_num = 0

        for env in current_envs:
            for node_env in node.label:
                pair_num += 1
                # union-env
                merged_assumptions = env.assumptions | node_env.assumptions
                new_env = get_env(merged_assumptions)

                print(f"  │")
                print(f"  │  [{pair_num}] {env} × {node_env}")
                print(f"  │      union: {_set_str(env.assumptions)} ∪ {_set_str(node_env.assumptions)}"
                      f" = {_set_str(merged_assumptions)}")
                print(f"  │      결과: {new_env}")

                # nogood 검사
                if new_env.is_nogood():
                    print(f"  │      ⚠️ NOGOOD → 차단!")
                    continue

                # antichain 삽입 (원본: atms.lisp:255-260)
                print(f"  │      antichain 삽입 검사:")
                dominated = False
                to_nullify = []

                for i, existing in enumerate(new_envs):
                    if existing is None:
                        continue
                    rel = compare_env(new_env, existing)
                    rel_str = _relation_str(rel, new_env, existing)

                    if rel in ('EQ', 'S21'):
                        print(f"  │        vs {existing}: {rel} — {rel_str}")
                        print(f"  │        → {new_env}는 지배당함 → 삽입 중단!")
                        dominated = True
                        break
                    elif rel == 'S12':
                        print(f"  │        vs {existing}: {rel} — {rel_str}")
                        print(f"  │        → {existing}를 nil로 표시 (나중에 제거)")
                        to_nullify.append(i)
                    else:
                        print(f"  │        vs {existing}: 비교불가 (incomparable)")

                if not dominated:
                    for i in to_nullify:
                        new_envs[i] = None
                    new_envs.append(new_env)
                    if to_nullify:
                        print(f"  │      ✓ 삽입 + 지배된 항목 제거")
                    else:
                        print(f"  │      ✓ 삽입")
                    print(f"  │      new_envs = {_list_str(new_envs)}")

        # delete nil
        new_envs = [e for e in new_envs if e is not None]

        print(f"  │")
        print(f"  │  단계 {step} 결과: {new_envs}")

        current_envs = new_envs

        if not current_envs:
            print(f"  │  ⚡ 빈 결과 → 조기 종료!")
            print(f"  └─ weave → None")
            return None

    print(f"  │")
    print(f"  └─ weave 최종 결과: {current_envs}")
    return current_envs


# ══════════════════════════════════════════════════════════════════
# 유틸리티
# ══════════════════════════════════════════════════════════════════

def _set_str(s: FrozenSet[str]) -> str:
    if not s:
        return "{∅}"
    return "{" + ", ".join(sorted(s)) + "}"

def _list_str(lst: List) -> str:
    items = []
    for e in lst:
        items.append(str(e) if e is not None else "nil")
    return "[" + ", ".join(items) + "]"

def _relation_str(rel, e1, e2) -> str:
    if rel == 'EQ':
        return f"{e1} = {e2} (동일)"
    elif rel == 'S12':
        return f"{_set_str(e1.assumptions)} ⊂ {_set_str(e2.assumptions)} (e1이 더 작아서 e2를 지배)"
    elif rel == 'S21':
        return f"{_set_str(e2.assumptions)} ⊂ {_set_str(e1.assumptions)} (e2가 더 작아서 e1을 지배)"
    else:
        return "비교불가"


# ══════════════════════════════════════════════════════════════════
# 실행
# ══════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    Env.reset()

    print("=" * 70)
    print("  weave 함수 디버깅 트레이스")
    print("=" * 70)
    print("""
  입력 조건:
    정당화: P ∧ Q → 결론
    P.label = [{A, 1}, {B}]
    Q.label = [{A, 2}, {B, 1}]
    antecedent = nil
    envs = [{∅}]
    """)

    # ── 환경 생성 ──
    E_empty = get_env(frozenset())

    # ── 노드 생성 ──
    P = Node("P")
    P.label = [get_env(frozenset({"A", "1"})),
               get_env(frozenset({"B"}))]

    Q = Node("Q")
    Q.label = [get_env(frozenset({"A", "2"})),
               get_env(frozenset({"B", "1"}))]

    print(f"  P.label = {P.label_str()}")
    print(f"  Q.label = {Q.label_str()}")
    print(f"  시작 envs = [{E_empty}]")
    print()

    # ── 교차곱 후보 미리보기 ──
    print("  ── 교차곱 후보 (P.label × Q.label = 2×2 = 4쌍) ──")
    print()
    candidates = []
    pair = 0
    for p_env in P.label:
        for q_env in Q.label:
            pair += 1
            merged = p_env.assumptions | q_env.assumptions
            candidates.append((p_env, q_env, merged))
            print(f"    [{pair}] {_set_str(p_env.assumptions)} ∪ {_set_str(q_env.assumptions)}"
                  f" = {_set_str(merged)}  (크기 {len(merged)})")

    print()
    print("  ── 최소성 예측 ──")
    print(f"    {_set_str(candidates[1][2])}(크기 3) ⊇ {_set_str(candidates[3][2])}(크기 2)")
    print(f"    → {{A,B,1}}은 {{B,1}}에 지배당해 제거될 것!")
    print(f"    예상 결과: [{{A,1,2}}, {{A,B,2}}, {{B,1}}]")
    print()

    # ── weave 실행 ──
    print("─" * 70)
    result = weave_trace(
        antecedent=None,
        envs=[E_empty],
        antecedents=[P, Q]
    )

    # ── 결과 분석 ──
    print()
    print("=" * 70)
    print("  결과 분석")
    print("=" * 70)

    if result is None:
        print("  weave → None (모든 경로가 차단됨)")
    else:
        print(f"\n  weave 결과: {len(result)}개의 최소 환경")
        print()
        for i, env in enumerate(result, 1):
            print(f"    [{i}] {env}  =  {_set_str(env.assumptions)}  (가정 {env.count}개)")

        print()
        print("  ── antichain 검증 (상호 비교불가) ──")
        all_ok = True
        for i in range(len(result)):
            for j in range(i + 1, len(result)):
                rel = compare_env(result[i], result[j])
                status = "✓ 비교불가" if rel is None else f"✗ {rel}!"
                print(f"    {result[i]} vs {result[j]}: {status}")
                if rel is not None:
                    all_ok = False
        if all_ok:
            print("    → 모든 쌍이 비교불가 — 유효한 antichain ✓")

        print()
        print("  ── 의미 해석 ──")
        print(f"    정당화 'P ∧ Q → 결론'이 참이 되는 최소 세계:")
        for i, env in enumerate(result, 1):
            assumes = sorted(env.assumptions)
            print(f"    세계 {i}: 가정 {', '.join(assumes)}가 모두 참이면 → 결론 참")

        print()
        print("  ── 제거된 후보 ──")
        result_sets = {e.assumptions for e in result}
        for p_env, q_env, merged in candidates:
            fs = frozenset(merged)
            if fs not in result_sets:
                dominator = None
                for r in result:
                    if r.assumptions < fs:
                        dominator = r
                        break
                print(f"    {_set_str(merged)} — 제거됨"
                      f" (∵ {dominator} ⊂ {_set_str(merged)})")
