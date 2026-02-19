"""
weave_fp.py — TCO Functional Programming version of weave
==========================================================

원본 Common Lisp weave 함수 (atms.lisp:245-263)를
Tail Call Optimization이 가능한 재귀적 함수형 프로그래밍 스타일로 변환합니다.

구조:
  Level 1 : fold_antecedents   — antecedents 리스트를 순회하는 tail-recursion
  Level 2a: process_node_envs  — 하나의 env × node.label 교차곱 tail-recursion
  Level 2b: process_envs       — 전체 envs × node.label 교차곱 tail-recursion
  Level 3 : insert_minimal     — antichain 삽입 tail-recursion

Python은 네이티브 TCO를 지원하지 않으므로 Trampoline 패턴을 사용합니다.
  - 각 tail-recursive 함수는 계속(continuation)이 필요한 경우 lambda thunk를 반환
  - trampoline()이 callable 결과를 반복 호출하여 stack overflow를 방지

원본 코드 (참고용):
  (defun weave (antecedent envs antecedents &aux new-envs new-env)
    (setq envs (copy-list envs))
    (dolist (node antecedents)
      (unless (eq node antecedent)
        (setq new-envs nil)
        (dolist (env envs)
          (if env
              (dolist (node-env (tms-node-label node))
                (setq new-env (union-env env node-env))
                (unless (env-nogood? new-env)
                  (do ((nnew-envs new-envs (cdr nnew-envs)))
                      ((null nnew-envs) (push new-env new-envs))
                    (when (car nnew-envs)
                      (case (compare-env new-env (car nnew-envs))
                        ((:EQ :S21) (return nil))
                        (:S12 (rplaca nnew-envs nil)))))))))
        (setq envs (delete nil new-envs :TEST #'eq))
        (unless envs (return-from weave nil))))
    envs)
"""

from __future__ import annotations
from typing import Optional, List, Callable, Any, FrozenSet
from dataclasses import dataclass, field


# ══════════════════════════════════════════════════════════════════
# 1. Trampoline 구현
# ══════════════════════════════════════════════════════════════════

def trampoline(f: Any) -> Any:
    """
    Trampoline 패턴: callable 결과를 계속 호출하여 stack overflow 방지.

    사용법:
      # tail-recursive 함수가 계속(continuation)이 필요할 때 lambda를 반환
      def count(n, acc=0):
          if n == 0: return acc              # 최종값 반환
          return lambda: count(n-1, acc+1)  # thunk 반환 (tail call)

      result = trampoline(lambda: count(1_000_000))  # → 1000000
    """
    result = f
    while callable(result):
        result = result()
    return result


# ══════════════════════════════════════════════════════════════════
# 2. ATMS 데이터 타입 시뮬레이션
# ══════════════════════════════════════════════════════════════════

class Env:
    """
    ATMS 환경(Environment) — 가능한 하나의 세계(possible world).
    assumptions: 이 세계에서 참(true)인 가정들의 집합.
    nogood:      이 환경이 모순(contradiction)임을 표시.
    """
    def __init__(self, assumptions: FrozenSet[str], nogood: bool = False):
        self.assumptions = assumptions
        self.nogood = nogood

    def __repr__(self) -> str:
        content = "+".join(sorted(self.assumptions)) if self.assumptions else "∅"
        return f"E{{{content}}}{'✗' if self.nogood else ''}"

    def __eq__(self, other) -> bool:
        return isinstance(other, Env) and self.assumptions == other.assumptions

    def __hash__(self) -> int:
        return hash(self.assumptions)


class Node:
    """
    ATMS 노드 — 하나의 명제(proposition).
    label: 이 노드가 지지(support)되는 minimal env들의 antichain.
    """
    def __init__(self, name: str, label: Optional[List[Env]] = None):
        self.name = name
        self.label = label if label is not None else []

    def __repr__(self) -> str:
        return self.name

    def __eq__(self, other) -> bool:
        return self is other  # identity comparison (eq in Lisp)

    def __hash__(self) -> int:
        return id(self)


# ══════════════════════════════════════════════════════════════════
# 3. 핵심 헬퍼 함수 (순수 함수)
# ══════════════════════════════════════════════════════════════════

# nogood 집합 (전역 상태 시뮬레이션 — 실제 ATMS에서는 atms-nogood-table)
_nogoods: List[FrozenSet[str]] = []

def register_nogood(assumptions: FrozenSet[str]) -> None:
    """nogood 환경으로 등록합니다."""
    _nogoods.append(assumptions)

def is_nogood(env: Optional[Env]) -> bool:
    """env가 None이거나 nogood인지 확인합니다."""
    if env is None:
        return True
    if env.nogood:
        return True
    # 등록된 nogood의 subset이면 nogood
    return any(ng <= env.assumptions for ng in _nogoods)

def union_env(e1: Env, e2: Env) -> Optional[Env]:
    """
    두 env를 합집합으로 병합합니다.
    결과가 nogood이면 None 반환.

    원본: (union-env e1 e2) → atms.lisp:307-314
    """
    merged_assumptions = e1.assumptions | e2.assumptions
    result = Env(merged_assumptions)
    if is_nogood(result):
        return None
    return result

def compare_env(e1: Env, e2: Env) -> Optional[str]:
    """
    두 env의 부분집합 관계를 비교합니다.

    반환값:
      'EQ'  : e1 == e2
      'S12' : e1 ⊂ e2  (e1이 더 작음, e1이 e2를 지배)
      'S21' : e2 ⊂ e1  (e2가 더 작음, e2가 e1을 지배)
      None  : 관계 없음

    원본: (compare-env e1 e2) → atms.lisp:356-363
    """
    a1, a2 = e1.assumptions, e2.assumptions
    if a1 == a2:   return 'EQ'
    if a1 < a2:    return 'S12'   # e1이 진부분집합 → e1이 e2를 지배
    if a2 < a1:    return 'S21'   # e2가 진부분집합 → e2가 e1을 지배
    return None


# ══════════════════════════════════════════════════════════════════
# 4. Level 3: insert_minimal — antichain 삽입 (TCO)
# ══════════════════════════════════════════════════════════════════

DOMINATED = object()  # sentinel: "새 env가 기존에 의해 지배됨, 삽입 불요"

def insert_minimal(new_env: Env, remaining: List[Env], kept: List[Env]) -> Any:
    """
    new_env를 antichain(remaining + kept)에 삽입합니다.
    antichain 불변식: 어떤 두 env도 부분집합 관계에 있지 않아야 함.

    원본의 do 루프:
      (do ((nnew-envs new-envs (cdr nnew-envs)))
          ((null nnew-envs) (push new-env new-envs))
        (when (car nnew-envs)
          (case (compare-env new-env (car nnew-envs))
            ((:EQ :S21) (return nil))       ; 지배당함 → 삽입 불요
            (:S12 (rplaca nnew-envs nil))   ; 지배함 → 기존 제거(nil 마킹)
          )))

    TCO 변환:
      - remaining : 아직 비교하지 않은 acc 원소
      - kept       : 비교 완료 후 유지할 원소 (역순 accumulator)
      - 반환값: DOMINATED sentinel | 새 antichain 리스트

    모든 재귀 호출이 꼬리 위치(tail position)에 있음 → TCO 가능.
    Python에서는 lambda thunk로 감싸 trampoline에 위임.
    """
    if not remaining:
        # 스캔 완료: new_env가 지배당하지 않음 → 앞에 추가
        # kept는 역순이므로 뒤집어서 이어붙임
        return [new_env] + kept[::-1]

    item = remaining[0]
    rel = compare_env(new_env, item)

    if rel in ('EQ', 'S21'):
        # new_env ⊇ item → new_env가 지배당함 → 삽입 불요
        return DOMINATED

    elif rel == 'S12':
        # new_env ⊂ item → item이 지배당함 → item 제거(kept에 추가 안 함)
        # 원본의 rplaca를 순수 함수적으로: 그냥 item을 건너뜀
        return lambda: insert_minimal(new_env, remaining[1:], kept)

    else:
        # 관계 없음 → item 유지 (kept에 추가)
        return lambda: insert_minimal(new_env, remaining[1:], [item] + kept)


# ══════════════════════════════════════════════════════════════════
# 5. Level 2a: process_node_envs — 하나의 env × node.label (TCO)
# ══════════════════════════════════════════════════════════════════

def process_node_envs(env: Env, remaining_node_label: List[Env], acc: List[Env]) -> Any:
    """
    env와 remaining_node_label의 각 node_env를 union_env로 결합하여
    valid한 결과를 acc에 누적합니다.

    원본의 내부 dolist:
      (dolist (node-env (tms-node-label node))
        (setq new-env (union-env env node-env))
        (unless (env-nogood? new-env) ...insert...))

    TCO: remaining_node_label을 소진하는 tail-recursion.
    """
    if not remaining_node_label:
        return acc

    node_env = remaining_node_label[0]
    merged = union_env(env, node_env)

    if is_nogood(merged):
        # nogood → 이 조합 건너뜀
        return lambda: process_node_envs(env, remaining_node_label[1:], acc)

    # valid → antichain 삽입 시도
    result = trampoline(lambda: insert_minimal(merged, acc, []))
    new_acc = acc if result is DOMINATED else result
    return lambda: process_node_envs(env, remaining_node_label[1:], new_acc)


# ══════════════════════════════════════════════════════════════════
# 6. Level 2b: process_envs — 전체 envs × node.label (TCO)
# ══════════════════════════════════════════════════════════════════

def process_envs(remaining_envs: List[Env], node_label: List[Env], acc: List[Env]) -> Any:
    """
    remaining_envs의 각 env에 대해 node_label과 교차곱(cross product)을 수행합니다.

    원본의 외부 dolist:
      (dolist (env envs) ...)
      → delete nil (null-marked envs 제거) 는 insert_minimal에서 처리됨

    TCO: remaining_envs를 소진하는 tail-recursion.
    """
    if not remaining_envs:
        return acc

    new_acc = trampoline(
        lambda: process_node_envs(remaining_envs[0], node_label, acc)
    )
    return lambda: process_envs(remaining_envs[1:], node_label, new_acc)


# ══════════════════════════════════════════════════════════════════
# 7. Level 1: fold_antecedents — antecedents 순회 (TCO)
# ══════════════════════════════════════════════════════════════════

def fold_antecedents(
    antecedent: Node,
    remaining: List[Node],
    current_envs: List[Env]
) -> Any:
    """
    antecedents 리스트를 순회하며 current_envs를 점진적으로 좁혀갑니다.

    원본의 최외곽 dolist:
      (dolist (node antecedents)
        (unless (eq node antecedent) ...)  ← 자기 자신 건너뜀
        (unless envs (return-from weave nil)))  ← 조기 종료

    TCO: remaining을 소진하는 tail-recursion.
    """
    if not remaining:
        return current_envs

    node = remaining[0]

    if node is antecedent:
        # 자기 자신 건너뜀: 증분 계산(incremental computation)의 핵심
        # antecedent의 envs는 이미 current_envs에 반영되어 있음
        return lambda: fold_antecedents(antecedent, remaining[1:], current_envs)

    # 이 node의 label과 교차곱 수행
    new_envs = trampoline(
        lambda: process_envs(current_envs, node.label, [])
    )

    if not new_envs:
        # 조기 종료: 가능한 세계가 없어짐 → 이 justification은 지지 불가
        return None

    # 다음 antecedent로 tail-recursion
    return lambda: fold_antecedents(antecedent, remaining[1:], new_envs)


# ══════════════════════════════════════════════════════════════════
# 8. weave_fp: 진입점
# ══════════════════════════════════════════════════════════════════

def weave_fp(
    antecedent: Node,
    envs: List[Env],
    antecedents: List[Node]
) -> Optional[List[Env]]:
    """
    TCO Functional Programming version of weave.

    원본 weave와 동일한 의미론(semantics):
      antecedent : 현재 propagate 중인 노드 (antecedents에서 건너뜀)
      envs       : antecedent의 현재 label (시작 env 집합)
      antecedents: justification의 전체 antecedent 목록

    반환: new minimal envs 리스트 (antichain), 또는 None

    구현 특징:
      - 상태 변이 없음 (no rplaca, no setq)
      - 모든 루프 → tail-recursive 내부 함수
      - Python trampoline으로 stack overflow 방지
    """
    return trampoline(
        lambda: fold_antecedents(antecedent, antecedents, list(envs))
    )


# ══════════════════════════════════════════════════════════════════
# 9. Mock 디버깅 테스트
# ══════════════════════════════════════════════════════════════════

def make_env(*assumptions: str) -> Env:
    return Env(frozenset(assumptions))


def run_tests():
    """
    이전 분석에서 설계한 4가지 mock 시나리오를 실행합니다.

    Mock 1: 기본 교차곱
    Mock 2: 최소성 필터 (:S21 — 새 env가 지배당함)
    Mock 3: 역방향 제거 (:S12 — 새 env가 기존을 지배)
    Mock 4: Nogood 차단
    """

    print("=" * 60)
    print("weave_fp Mock 디버깅 테스트")
    print("=" * 60)

    # ──────────────────────────────────────────────────────────────
    # Mock 1: 기본 교차곱
    # 세 노드 A, B, C가 각각 단일 env를 가질 때
    # A의 env와 B, C의 env를 교차하면 {A,B,C}가 나와야 함
    # ──────────────────────────────────────────────────────────────
    print("\n[Mock 1] 기본 교차곱")
    print("  A.label = [E{a}], B.label = [E{b}], C.label = [E{c}]")
    print("  antecedent = A, antecedents = [A, B, C]")
    print("  기대값: [E{a+b+c}]")

    _nogoods.clear()
    A = Node("A", [make_env("a")])
    B = Node("B", [make_env("b")])
    C = Node("C", [make_env("c")])

    result = weave_fp(A, A.label, [A, B, C])
    print(f"  결과:   {result}")
    expected = [make_env("a", "b", "c")]
    assert result == expected, f"실패! {result} != {expected}"
    print("  ✓ PASS")

    # ──────────────────────────────────────────────────────────────
    # Mock 2: 최소성 필터 — :S21 케이스
    # B.label이 E{b}와 E{a,b} 두 개를 가질 때:
    #   - A와 B{b} 결합 → E{a,b}
    #   - A와 B{a,b} 결합 → E{a,b} (같음 :EQ → 중복, DOMINATED)
    # 결과: [E{a,b}] (중복 없음)
    # ──────────────────────────────────────────────────────────────
    print("\n[Mock 2] 최소성 필터 (:S21 — 지배당해 삽입 불요)")
    print("  A.label = [E{a}], B.label = [E{b}, E{a,b}]")
    print("  기대값: [E{a+b}] (E{a,b}는 E{a,b}에 지배당해 중복 제거)")

    _nogoods.clear()
    A2 = Node("A", [make_env("a")])
    B2 = Node("B", [make_env("b"), make_env("a", "b")])

    result2 = weave_fp(A2, A2.label, [A2, B2])
    print(f"  결과:   {result2}")
    assert result2 == [make_env("a", "b")], f"실패! {result2}"
    print("  ✓ PASS")

    # ──────────────────────────────────────────────────────────────
    # Mock 3: 역방향 제거 — :S12 케이스
    # B.label = [E{a,b,x}, E{b}]  (더 큰 env가 먼저 들어온 경우)
    # A.label = [E{a}]
    # 처리 순서:
    #   1. A{a} ∪ B{a,b,x} → E{a,b,x} → acc=[E{a,b,x}]
    #   2. A{a} ∪ B{b}     → E{a,b}   → compare E{a,b} vs E{a,b,x} → S12
    #      → E{a,b,x}을 acc에서 제거, E{a,b} 추가 → acc=[E{a,b}]
    # 결과: [E{a,b}]
    # ──────────────────────────────────────────────────────────────
    print("\n[Mock 3] 역방향 제거 (:S12 — 새 env가 기존을 지배)")
    print("  A.label = [E{a}], B.label = [E{a,b,x}, E{b}]")
    print("  기대값: [E{a+b}] (E{a,b,x}는 E{a,b}에 지배당해 제거)")

    _nogoods.clear()
    A3 = Node("A", [make_env("a")])
    B3 = Node("B", [make_env("a", "b", "x"), make_env("b")])

    result3 = weave_fp(A3, A3.label, [A3, B3])
    print(f"  결과:   {result3}")
    assert result3 == [make_env("a", "b")], f"실패! {result3}"
    print("  ✓ PASS")

    # ──────────────────────────────────────────────────────────────
    # Mock 4: Nogood 차단
    # nogood = {a, b} (a와 b를 동시에 가정하면 모순)
    # A.label = [E{a}], B.label = [E{b}]
    # A{a} ∪ B{b} → E{a,b} → is_nogood? YES → 차단
    # 결과: None (가능한 세계 없음)
    # ──────────────────────────────────────────────────────────────
    print("\n[Mock 4] Nogood 차단")
    print("  nogood = {a,b}  (a와 b를 동시에 가정하면 모순)")
    print("  A.label = [E{a}], B.label = [E{b}]")
    print("  기대값: None (모든 조합이 nogood)")

    _nogoods.clear()
    register_nogood(frozenset({"a", "b"}))
    A4 = Node("A", [make_env("a")])
    B4 = Node("B", [make_env("b")])

    result4 = weave_fp(A4, A4.label, [A4, B4])
    print(f"  결과:   {result4}")
    assert result4 is None, f"실패! {result4}"
    print("  ✓ PASS")

    # ──────────────────────────────────────────────────────────────
    # Mock 5: 부분적 nogood — 일부만 차단
    # nogood = {a, b}
    # A.label = [E{a}], B.label = [E{b}, E{c}]
    # A{a} ∪ B{b} → E{a,b} → nogood → 차단
    # A{a} ∪ B{c} → E{a,c} → valid  → 삽입
    # 결과: [E{a,c}]
    # ──────────────────────────────────────────────────────────────
    print("\n[Mock 5] 부분적 Nogood 차단")
    print("  nogood = {a,b}, A.label = [E{a}], B.label = [E{b}, E{c}]")
    print("  기대값: [E{a+c}]  (E{a,b}는 차단, E{a,c}는 valid)")

    _nogoods.clear()
    register_nogood(frozenset({"a", "b"}))
    A5 = Node("A", [make_env("a")])
    B5 = Node("B", [make_env("b"), make_env("c")])

    result5 = weave_fp(A5, A5.label, [A5, B5])
    print(f"  결과:   {result5}")
    assert result5 == [make_env("a", "c")], f"실패! {result5}"
    print("  ✓ PASS")

    print("\n" + "=" * 60)
    print("모든 테스트 통과 ✓")
    print("=" * 60)


# ══════════════════════════════════════════════════════════════════
# 10. 원본 weave와 weave_fp 구조 비교 요약
# ══════════════════════════════════════════════════════════════════

DESIGN_NOTES = """
┌─────────────────────────────────────────────────────────────────┐
│           원본 weave vs weave_fp 변환 매핑                       │
├────────────────────┬────────────────────────────────────────────┤
│ 원본 (명령형)       │ weave_fp (함수형 TCO)                      │
├────────────────────┼────────────────────────────────────────────┤
│ setq new-envs nil  │ acc=[] (accumulator로 초기화)               │
│ dolist antecedents │ fold_antecedents (tail-recursion)          │
│ dolist envs        │ process_envs (tail-recursion)              │
│ dolist node-label  │ process_node_envs (tail-recursion)         │
│ do nnew-envs 루프  │ insert_minimal (tail-recursion)            │
│ rplaca ... nil     │ kept 목록에서 제외 (순수 함수적 삭제)         │
│ delete nil         │ kept[::-1]로 재구성 (compact 불필요)         │
│ return-from weave  │ None 반환 (조기 종료 신호)                   │
│ :EQ/:S21 → return  │ DOMINATED sentinel 반환                    │
│ copy-list envs     │ list(envs) (불변 복사)                      │
├────────────────────┼────────────────────────────────────────────┤
│ TCO 가능 여부      │                                            │
│ 원본: 불가 (do 등  │ 모든 재귀 호출이 꼬리 위치(tail position)    │
│ 반복문 내 변이)     │ Python: trampoline으로 stack 관리            │
│                    │ CL: declare optimize speed (TCO 활성화)     │
│                    │ Haskell: 기본적으로 TCO 최적화               │
└────────────────────┴────────────────────────────────────────────┘

핵심 설계 결정:
  1. DOMINATED sentinel: None 대신 별도 객체 사용
     → None이 "빈 리스트"와 "삽입 불요"를 구분 불가능하기 때문
  2. kept 역순 accumulator: 원래 순서 보존을 위해 kept[::-1]
     → ATMS에서 env 순서는 의미에 영향을 주지 않지만 테스트 편의상
  3. trampoline 중첩: process_node_envs 내부에서 insert_minimal 호출 시
     별도 trampoline() 사용 → 스택이 process_node_envs와 독립적으로 관리
"""


# ══════════════════════════════════════════════════════════════════
# 11. Haskell 참고 구현 (주석)
# ══════════════════════════════════════════════════════════════════

HASKELL_VERSION = """
-- ================================================================
-- Haskell TCO version of weave
-- Haskell은 네이티브 TCO를 지원하므로 trampoline 불필요
-- GHC가 tail-recursive 패턴을 자동으로 최적화
-- ================================================================

module WeaveAtms where
import Data.List (nub)
import qualified Data.Set as Set

type Assumptions = Set.Set String
data Env = Env { envAssumptions :: Assumptions, envNogood :: Bool }
           deriving (Show, Eq, Ord)

type Label = [Env]

data CompareResult = EQ_ | S12 | S21 | NoRel deriving Eq

compareEnv :: Env -> Env -> CompareResult
compareEnv e1 e2
  | a1 == a2        = EQ_
  | a1 `Set.isSubsetOf` a2 = S12  -- e1 ⊂ e2: e1이 지배
  | a2 `Set.isSubsetOf` a1 = S21  -- e2 ⊂ e1: e2가 지배
  | otherwise       = NoRel
  where a1 = envAssumptions e1; a2 = envAssumptions e2

unionEnv :: Env -> Env -> Maybe Env
unionEnv e1 e2 =
  let merged = Env (envAssumptions e1 `Set.union` envAssumptions e2) False
  in if isNogood merged then Nothing else Just merged

isNogood :: Env -> Bool  -- 실제 구현에서는 ATMS nogood-table 참조
isNogood env = envNogood env

-- Level 3: insert_minimal (자연스럽게 TCO)
data InsertResult = Dominated | Inserted Label

insertMinimal :: Env -> Label -> [Env] -> InsertResult
insertMinimal newEnv [] kept       = Inserted (newEnv : reverse kept)
insertMinimal newEnv (item:rest) kept =
  case compareEnv newEnv item of
    EQ_  -> Dominated                              -- 지배당함
    S21  -> Dominated                              -- 지배당함
    S12  -> insertMinimal newEnv rest kept         -- item 제거 후 계속 (TCO)
    _    -> insertMinimal newEnv rest (item:kept)  -- item 유지 후 계속 (TCO)

-- Level 2a: process_node_envs (TCO)
processNodeEnvs :: Env -> Label -> Label -> Label
processNodeEnvs _ [] acc = acc
processNodeEnvs env (nodeEnv:rest) acc =
  case unionEnv env nodeEnv of
    Nothing     -> processNodeEnvs env rest acc  -- nogood (TCO)
    Just merged ->
      let newAcc = case insertMinimal merged acc [] of
                     Dominated       -> acc
                     Inserted newAcc -> newAcc
      in processNodeEnvs env rest newAcc          -- (TCO)

-- Level 2b: process_envs (TCO)
processEnvs :: Label -> Label -> Label -> Label
processEnvs [] _ acc = acc
processEnvs (env:rest) nodeLabel acc =
  let newAcc = processNodeEnvs env nodeLabel acc
  in processEnvs rest nodeLabel newAcc             -- (TCO)

-- Level 1: fold_antecedents (TCO)
data WeaveResult = NoWorlds | Worlds Label

foldAntecedents :: Env -> [Env] -> [Env] -> [Env] -> Maybe Label
-- antecedent이름을 Node로 추상화했지만 여기서는 단순화
foldAntecedents _ [] currentEnvs _  = Just currentEnvs
foldAntecedents antNode (node:rest) currentEnvs allNodes =
  let newEnvs = processEnvs currentEnvs (nodeLabel node) []
  in if null newEnvs
       then Nothing                                -- 조기 종료
       else foldAntecedents antNode rest newEnvs allNodes  -- (TCO)

-- weave_fp: 진입점
weaveFp :: Env -> Label -> [Env] -> Maybe Label
weaveFp antecedent envs antecedents =
  foldAntecedents antecedent antecedents envs antecedents
"""


if __name__ == "__main__":
    run_tests()
    print(DESIGN_NOTES)
