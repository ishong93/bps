# LTMS (Logic-based Truth Maintenance System) 코드 구조 분석

> "Building Problem Solvers" (Forbus & de Kleer) 교재의 LTMS 구현체 분석

---

## 1. LTMS란 무엇인가?

LTMS는 **논리 기반 진리 유지 시스템**(Logic-based Truth Maintenance System)으로,
명제(proposition)들의 참/거짓 상태를 **일관성 있게 유지**하면서 추론을 수행하는 시스템이다.

핵심 역할:
- 가정(assumption)을 세우고 철회할 수 있음
- 절(clause) 형태의 제약 조건을 관리
- **BCP(Boolean Constraint Propagation)** 로 자동 추론
- 모순 발생 시 원인을 추적하고 해결

---

## 2. 전체 파일 구조 (15개 파일, 약 3,552줄)

```
ltms/
├── ltms.lisp        (746줄) 핵심 엔진: 노드, 절, BCP, 역전파
├── cltms.lisp       (426줄) 완전 LTMS: 해소(resolution), 트라이 인덱싱
├── linter.lisp      (101줄) LTRE 정의: LTMS + 데이터베이스 통합
├── ldata.lisp       (305줄) 데이터베이스: 사실/가정 관리
├── lrules.lisp      (286줄) 규칙 엔진: 패턴 매칭 기반 트리거
├── funify.lisp      (131줄) 패턴 통합(unification)
├── cwa.lisp         (225줄) 닫힌 세계 가정(Closed-World Assumption)
├── dds.lisp          (83줄) 의존성 지향 탐색(Dependency-Directed Search)
├── indirect.lisp     (47줄) 간접 증명(Indirect Proof)
├── setrule.lisp      (41줄) 집합 제약 규칙
├── ltms-ex.lisp     (896줄) 종합 테스트/예제
├── marx.lisp         (54줄) 마르크스 형제 퍼즐 예제
├── marxdata.lisp     (81줄) 마르크스 형제 데이터
├── laccept.lisp      (79줄) 인수 테스트
└── ltre.lisp         (51줄) LTRE 로딩 유틸리티
```

---

## 3. 핵심 자료구조

### 3.1 LTMS 구조체 (`ltms.lisp:18-35`)

```lisp
(defstruct ltms
  title                        ; 시스템 이름
  (node-counter 0)             ; 노드 고유 번호
  (clause-counter 0)           ; 절 고유 번호
  (nodes nil)                  ; 노드 해시 테이블
  (clauses nil)                ; 모든 절의 리스트
  (checking-contradictions t)  ; 모순 검사 활성화 여부
  (contradiction-handlers nil) ; 모순 처리기 목록
  (violated-clauses nil)       ; 위반된 절들
  (queue nil)                  ; 해소 대기 큐
  (complete nil)               ; 완전 LTMS 모드 여부
  ...)
```

### 3.2 TMS 노드 (`ltms.lisp:37-50`)

각 **명제**(proposition)를 표현하는 노드:

```lisp
(defstruct tms-node
  (index 0)            ; 고유 식별자
  (datum nil)          ; 추론 엔진 데이터
  (label :UNKNOWN)     ; 상태: :UNKNOWN | :TRUE | :FALSE
  (support nil)        ; 이 노드를 지지하는 절(clause)
  (true-clauses nil)   ; 이 노드가 참인 절 목록
  (false-clauses nil)  ; 이 노드가 거짓인 절 목록
  (assumption? nil)    ; 가정(assumption) 여부
  (true-rules nil)     ; 참이 될 때 실행할 규칙들
  (false-rules nil))   ; 거짓이 될 때 실행할 규칙들
```

노드의 3가지 상태:
- `:UNKNOWN` - 아직 참/거짓이 결정되지 않음
- `:TRUE` - 참으로 결정됨
- `:FALSE` - 거짓으로 결정됨

### 3.3 절(Clause) (`ltms.lisp:57-64`)

**CNF(Conjunctive Normal Form) 절**: 리터럴들의 논리합(OR)

```lisp
(defstruct clause
  (index 0)         ; 고유 식별자
  (informant nil)   ; 출처 정보 (누가 이 절을 추가했는가)
  (literals nil)    ; 리터럴 리스트: ((node1 . :TRUE) (node2 . :FALSE) ...)
  (pvs 0)           ; "Potentially Violating" 항 수 (아직 미결정된 리터럴 수)
  (sats 0)          ; 만족시키는 항 수 (이미 참인 리터럴 수)
  (length 0)        ; 절의 전체 길이
  (status nil))     ; :SUBSUMED | :QUEUED | :DIRTY
```

**핵심 개념 - `pvs` 카운터**:
절 내에서 아직 해당 절을 **위반할 가능성이 있는** 리터럴의 수.
- `pvs = 0` → 모든 리터럴이 거짓 → **모순(contradiction)!**
- `pvs = 1` → 딱 하나만 남음 → **단위 전파(unit propagation)** 수행

---

## 4. 핵심 알고리즘

### 4.1 BCP (Boolean Constraint Propagation)

LTMS의 심장부. 변수의 값이 결정될 때 연쇄적으로 다른 변수를 결정한다.

```
check-clauses(ltms):
  큐에서 절을 하나씩 꺼내서:
    만약 pvs = 0 이면:
      → 위반된 절로 등록 (모순 발생)
    만약 pvs = 1 이면:
      → 남은 하나의 미결정 리터럴을 찾아서
      → 해당 리터럴이 참이 되도록 set-truth 호출 (단위 전파)
```

### 4.2 Set-Truth (진리값 설정 및 전파)

```
set-truth(node, value, reason):
  1. node의 label을 value(:TRUE 또는 :FALSE)로 설정
  2. node의 support를 reason(근거 절)으로 기록
  3. 관련 절들의 pvs/sats 카운터 업데이트:
     - value가 :TRUE이면:
       - true-clauses의 sats 증가 (만족됨)
       - false-clauses의 pvs 감소 (가능성 줄어듦)
     - value가 :FALSE이면:
       - false-clauses의 sats 증가
       - true-clauses의 pvs 감소
  4. pvs < 2인 절은 검사 큐에 추가
  5. 연쇄 전파 계속
```

### 4.3 Propagate-Unknownness (역전파 / 가정 철회)

가정을 철회하면, 그 가정에 의존한 모든 결론을 되돌린다:

```
propagate-unknownness(node):
  1. node의 label을 :UNKNOWN으로 복원
  2. 이 node에 의존하던 다른 절들의 pvs/sats 복원
  3. pvs가 1→2가 된 절의 종전 결론(consequent)도 :UNKNOWN으로 복원
  4. 연쇄적으로 모든 의존 추론을 무효화
  5. 다시 전파하여 새로운 안정 상태 도달
```

---

## 5. 수식 정규화 (Formula Normalization)

사용자가 입력한 논리식을 CNF 절로 변환한다:

```lisp
;; 입력: (:IMPLIES (:AND r (:IMPLIES s t)) u)
;; 의미: (r ∧ (s → t)) → u

;; 변환 과정:
;; 1. 함의 제거: ¬(r ∧ (¬s ∨ t)) ∨ u
;; 2. 드모르간: (¬r ∨ (s ∧ ¬t)) ∨ u
;; 3. 분배법칙 적용 → CNF 절 집합
```

지원하는 논리 연산자:
- `:AND` - 논리곱
- `:OR` - 논리합
- `:NOT` - 부정
- `:IMPLIES` - 함의 (A→B ≡ ¬A∨B)
- `:IFF` - 동치 (A↔B ≡ (A→B)∧(B→A))
- `:TAXONOMY` - 상호 배타적 선택 (정확히 하나만 참)

---

## 6. 해결하고자 하는 과제 - 구체적 예제

### 예제 1: 추론 연쇄 (test-explain)

**문제**: x가 거짓일 때, r의 진리값은?

```lisp
(defun test-explain ()
  ;; 절 1: x ∨ y         (x 또는 y는 참)
  ;; 절 2: ¬y ∨ z        (y이면 z)
  ;; 절 3: ¬z ∨ r        (z이면 r)
  ;; 가정: x = FALSE

  (enable-assumption x :FALSE))
```

**LTMS의 추론 과정**:
```
1단계: x = FALSE (가정)
  → 절 1에서 pvs가 줄어듦 → pvs=1 → 단위 전파
  → y = TRUE (x∨y에서 x가 거짓이므로 y는 반드시 참)

2단계: y = TRUE
  → 절 2에서 (¬y ∨ z), ¬y가 거짓 → pvs=1
  → z = TRUE

3단계: z = TRUE
  → 절 3에서 (¬z ∨ r), ¬z가 거짓 → pvs=1
  → r = TRUE

결과: x=FALSE → y=TRUE → z=TRUE → r=TRUE
```

이 예제가 보여주는 것: **LTMS가 하나의 가정으로부터 연쇄적으로 모든 결론을 자동 도출**하는 능력.

---

### 예제 2: 모순 감지와 처리 (test-ask)

**문제**: n1=FALSE, n2=FALSE인데 "n1 또는 n2"라는 제약이 있으면?

```lisp
(defun test-ask ()
  ;; n1, n2는 가정(assumption)
  (enable-assumption n1 :FALSE)
  (enable-assumption n2 :FALSE)
  ;; 절: n1 ∨ n2 (둘 중 하나는 참이어야 함)
  (compile-formula *ltms* '(:or n1 n2)))
```

**LTMS의 처리 과정**:
```
상태: n1=FALSE, n2=FALSE
절 (n1 ∨ n2) 추가 시:
  → n1이 FALSE → sats 변화 없음, pvs 감소
  → n2이 FALSE → pvs 감소
  → pvs = 0 → 모순 발생!

모순 핸들러 호출:
  → "n1=FALSE와 n2=FALSE 중 어느 가정을 철회하시겠습니까?"
  → 사용자에게 선택지 제시
```

이 예제가 보여주는 것: **모순 자동 감지**와 **가정 기반 원인 추적**.

---

### 예제 3: 가정 철회와 재추론 (test-bug)

**문제**: 가정을 철회하면 이전 결론은 어떻게 되는가?

```lisp
(defun test-bug ()
  ;; 절 1: x ∨ z
  ;; 절 2: y ∨ z
  ;; 가정: x=FALSE, y=FALSE
  (enable-assumption x :FALSE)   ; → z=TRUE (절1에서 단위 전파)
  (enable-assumption y :FALSE)   ; y의 값만 설정, z는 이미 TRUE

  ;; 이 시점: x=FALSE, y=FALSE, z=TRUE
  (retract-assumption x)         ; x의 가정을 철회하면?
```

**LTMS의 처리 과정**:
```
x 가정 철회:
  1. x → :UNKNOWN
  2. z는 x로부터 유도되었으므로 → :UNKNOWN?
     → propagate-unknownness 실행
  3. 하지만 절 2 (y ∨ z)에서 y=FALSE이므로
     → z는 여전히 TRUE가 되어야 함
  4. 재전파: z = TRUE (이번에는 절 2가 근거)

결과: x=UNKNOWN, y=FALSE, z=TRUE
z의 support가 절1에서 절2로 바뀜
```

이 예제가 보여주는 것: **비단조적 추론(non-monotonic reasoning)** — 가정 철회 시 의존 관계를 정확히 추적하여 필요한 것만 되돌리고, 다른 근거가 있으면 결론을 유지.

---

### 예제 4: 마르크스 형제 퍼즐 (제약 만족 문제)

**문제**: 다음 단서로 Groucho, Harpo, Chico의 속성을 결정하라.

```
단서:
1. 피아니스트, 하프 연주자, 말잘하는 사람은 서로 다른 형제
2. 돈 좋아하는 사람, 도박 좋아하는 사람, 동물 좋아하는 사람은 서로 다름
3. 말잘하는 사람은 도박을 싫어함
4. 동물 좋아하는 사람은 하프를 연주함
5. Groucho는 동물을 싫어함
6. Harpo는 항상 말이 없음
7. Chico는 피아노를 연주함
```

**LTMS + DDS(의존성 지향 탐색)의 풀이 과정**:

```
1단계: 선택 집합 생성
  plays-piano: [(plays-piano groucho), (plays-piano harpo), (plays-piano chico)]
  plays-harp:  [(plays-harp groucho), (plays-harp harpo), (plays-harp chico)]
  ...

2단계: 제약을 논리식으로 표현
  (pairwise-nogood plays-piano plays-harp)
    → 같은 사람이 피아노와 하프를 둘 다 연주할 수 없음
  (same-entity likes-animals plays-harp)
    → 동물 좋아하는 사람 = 하프 연주자
  (:not (likes-animals groucho))
    → Groucho는 동물을 안 좋아함

3단계: DDS가 가정을 세우고 탐색
  가정: (plays-piano groucho) → 규칙 발동 → 모순?
    → (plays-piano chico)가 이미 참이고 pairwise-nogood이므로 모순!
    → 이 가정 철회, nogood 학습

  가정: (plays-piano harpo) → 모순 (같은 이유)
  가정: (plays-piano chico) → 참! (이미 알려진 사실)

  계속 탐색...

최종 결과:
  Chico: plays-piano, likes-gambling
  Harpo: plays-harp, likes-animals
  Groucho: smooth-talker, likes-money
```

이 예제가 보여주는 것:
- **제약 만족 문제(CSP)** 를 논리적으로 모델링
- **의존성 지향 탐색**: 모순 시 관련 가정만 골라서 철회 (단순 백트래킹보다 효율적)
- **Nogood 학습**: 같은 실패를 반복하지 않음

---

### 예제 5: 완전 LTMS와 해소(Resolution) (test1)

```lisp
(defun test1 (&optional (complete T))
  ;; 절 1: x ∨ y
  ;; 절 2: x ∨ ¬y
  (add-formula *ltms* '(:or x y))
  (add-formula *ltms* '(:or x (:not y)))
  (complete-ltms *ltms*))
```

**완전 LTMS의 해소 과정**:
```
절 1: x ∨ y
절 2: x ∨ ¬y

해소(resolution): y와 ¬y를 상쇄
→ 새로운 절: x (단위 절)
→ x = TRUE (자동 도출)

어떤 가정도 없이, 순수하게 절들의 해소만으로 x가 참임을 증명!
```

이 예제가 보여주는 것: **완전 추론** — 기본 BCP로는 알 수 없는 사실도 해소(resolution)를 통해 도출.

---

## 7. LTMS가 해결하는 과제 요약

| 과제 | 설명 | 핵심 메커니즘 |
|------|------|-------------|
| **자동 추론** | 하나의 사실로부터 연쇄적 결론 도출 | BCP + 단위 전파 |
| **모순 감지** | 일관성 없는 지식 자동 발견 | pvs=0 감지 |
| **가정 관리** | 가정을 세우고 철회하며 what-if 분석 | enable/retract assumption |
| **비단조 추론** | 가정 철회 시 의존 결론만 정확히 되돌림 | propagate-unknownness |
| **원인 추적** | 모순의 원인이 되는 가정 식별 | assumptions-of-clause |
| **제약 만족** | CSP를 논리적으로 모델링하고 풀기 | DDS + nogood 학습 |
| **완전 추론** | 해소를 통한 모든 도출 가능한 사실 발견 | CLTMS + resolution |
| **닫힌 세계 가정** | "알려지지 않은 것은 거짓" 처리 | CWA 메커니즘 |

---

## 8. 아키텍처 다이어그램

```
┌─────────────────────────────────────────────────┐
│                    LTRE                          │
│  ┌──────────────┐  ┌───────────┐  ┌──────────┐  │
│  │  규칙 엔진    │  │ 데이터베이스│  │ 패턴매칭  │  │
│  │ (lrules.lisp)│  │(ldata.lisp)│  │(funify)  │  │
│  └──────┬───────┘  └─────┬─────┘  └────┬─────┘  │
│         │                │              │        │
│  ┌──────┴────────────────┴──────────────┴─────┐  │
│  │              LTMS 핵심 엔진                  │  │
│  │  ┌─────────┐  ┌───────┐  ┌──────────────┐  │  │
│  │  │  노드   │──│  절   │──│  BCP / 전파   │  │  │
│  │  └─────────┘  └───────┘  └──────────────┘  │  │
│  │  ┌─────────────────┐  ┌─────────────────┐  │  │
│  │  │ 모순 처리기      │  │ 가정 관리       │  │  │
│  │  └─────────────────┘  └─────────────────┘  │  │
│  └────────────────────────────────────────────┘  │
│                                                  │
│  ┌────────────┐  ┌──────┐  ┌────────────────┐   │
│  │ DDS (탐색)  │  │ CWA  │  │ 간접증명       │   │
│  └────────────┘  └──────┘  └────────────────┘   │
└─────────────────────────────────────────────────┘
```

---

## 9. 결론

LTMS는 단순한 데이터 저장소가 아니라, **지식의 일관성을 능동적으로 관리하는 추론 엔진**이다.

핵심 가치:
1. **효율성**: BCP를 통한 빠른 제약 전파 (SAT 솔버의 기초)
2. **정확성**: 의존성 추적으로 정확한 추론 유지/철회
3. **유연성**: 가정 기반 what-if 분석으로 탐색 공간 효율적 탐색
4. **확장성**: 규칙 엔진, CWA, DDS 등 다양한 추론 전략과 통합

현대 AI 시스템에서 SAT 솔버, SMT 솔버, 지식 그래프 등의 기초가 되는 핵심 개념들이
이 LTMS 코드에 구현되어 있다.
