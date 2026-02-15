;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;; Assumption-based truth maintenance system, version 61 of 7/21/92.

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; ================================================================
;;; 데이터 구조 정의
;;; ================================================================
;;; ATMS의 4대 핵심 구조체:
;;;   atms      - 시스템 전체를 관리하는 최상위 컨테이너
;;;   tms-node  - 추론의 대상이 되는 명제(proposition) 노드
;;;   just      - "전건들이 참이면 결론도 참"이라는 정당화(justification) 규칙
;;;   env       - 가정(assumption)들의 집합 = 하나의 "세계관"
;;;
;;; 사용 예:
;;;   (setq my-atms (create-atms "진단 시스템"))
;;;   ;; → #<ATMS: 진단 시스템>
;;; ================================================================

;;; ATMS 구조체: 전체 시스템의 상태를 담는 최상위 객체
;;; 모든 노드, 정당화, 환경, nogood 정보를 중앙에서 관리한다.
(defstruct (atms (:PRINT-FUNCTION print-atms))
  (title nil)                   ; ATMS 인스턴스의 이름. 예: "회로 진단"
  (node-counter 0)              ; 노드 고유 번호 카운터. 새 노드마다 1씩 증가.
  (just-counter 0)              ; 정당화 고유 번호 카운터.
  (env-counter 0)               ; 환경 고유 번호 카운터.
  (nodes nil)                   ; 생성된 모든 tms-node의 리스트.
  (justs nil)                   ; 생성된 모든 정당화(just)의 리스트.
  (contradictions nil)          ; 모순으로 표시된 노드들의 리스트.
  (assumptions nil)             ; 가정(assumption)으로 표시된 노드들의 리스트.
  (debugging nil)               ; t이면 추론 과정을 trace-output에 출력.
  (nogood-table nil)            ; 모순 환경(nogood) 테이블. 가정 개수별로 분류.
                                ; 예: ((1 E3) (2 E5 E7)) → 크기1 nogood: E3, 크기2: E5,E7
  (contra-node nil)             ; 모순을 나타내는 더미 노드. 모든 nogood 정당화의 결론이 됨.
  (env-table nil)               ; 전체 환경 테이블. 가정 개수별로 분류 저장.
                                ; 예: ((0 E0) (1 E1 E2) (2 E4)) → 크기별 환경 버킷
  (empty-env nil)               ; 빈 환경 {}. 가정 없이도 참인 것들의 기반.
  (node-string nil)             ; 노드를 문자열로 변환하는 함수 (외부 제공).
  (enqueue-procedure nil))      ; label 변경 시 외부 추론엔진에 규칙 실행을 알리는 콜백.

(defun print-atms (atms stream ignore)
  (declare (ignore ignore))
  (format stream "#<ATMS: ~A>" (atms-title atms)))

;;; TMS-NODE 구조체: 추론 대상이 되는 하나의 명제
;;; 예: "X=6", "부품M1이 정상", "환자가 기침함"
;;;
;;; 가장 중요한 필드는 label이다:
;;;   label = 이 노드를 참으로 만드는 최소 환경들의 리스트
;;;
;;; 예시:
;;;   노드 "기침"이 감기(A1) 또는 알레르기(A2) 때문에 참일 수 있으면:
;;;   label = [{A1}, {A2}]
;;;   → "A1만 가정해도 기침이 참, A2만 가정해도 기침이 참"
(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)                     ; 노드 고유 번호 (1부터 자동 부여).
  (datum nil)                   ; 외부 추론엔진이 연결한 데이터. 예: "X=6"
  (label nil)                   ; ★ 핵심 ★ 이 노드를 참으로 만드는 최소 환경들의 리스트.
                                ; 예: ({A1}, {A2}) → 두 가지 독립적 근거가 있음.
                                ; 항상 최소성(minimality) 유지: {A1}이 있으면 {A1,A2}는 불필요.
  (justs nil)                   ; 이 노드를 결론으로 하는 정당화 리스트 (나를 지지하는 규칙들).
  (consequences nil)            ; 이 노드를 전건으로 사용하는 정당화 리스트 (내가 지지하는 규칙들).
  (contradictory? nil)          ; t이면 이 노드는 모순을 나타냄.
                                ; label에 환경이 추가되면 그 환경은 자동으로 nogood가 됨.
  (assumption? nil)             ; t이면 이 노드는 가정(assumption)임.
                                ; 가정은 외부에서 "참이라고 가정"한 것, 정당화 없이 참.
  (rules nil)                   ; label이 비어있다가 비어있지 않게 될 때 실행할 규칙 리스트.
  (atms nil))                   ; 이 노드가 속한 ATMS 인스턴스.

;;; 노드 출력: 가정이면 "A-번호", 일반 노드면 "#<NODE: datum>"
;;; 예: 가정 노드 index=3 → "A-3", 일반 노드 datum="X=6" → "#<NODE: X=6>"
(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (if (tms-node-assumption? node)
      (format stream "A-~D" (tms-node-index node))
      (format stream "#<NODE: ~A>" (node-string node))))

;;; JUST 구조체: 정당화(justification) = 추론 규칙 하나
;;; "전건(antecedents)이 모두 참이면, 결론(consequence)도 참이다"
;;;
;;; 예시:
;;;   informant   = "곱셈규칙"
;;;   antecedents = (노드-A=3, 노드-B=2, 노드-M1정상)
;;;   consequence = 노드-X=6
;;;   → "A=3이고 B=2이고 M1이 정상이면, X=6이다"
(defstruct (just (:PRINT-FUNCTION print-just))
	   (index 0)            ; 정당화 고유 번호.
	   (informant nil)      ; 이 규칙을 만든 주체의 이름. 예: "곱셈규칙", "관측"
	   (consequence nil)    ; 결론 노드. 예: 노드 "X=6"
	   (antecedents nil))   ; 전건 노드 리스트. 예: (노드-A, 노드-B). 빈 리스트면 무조건 참.

(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "<~A ~D>" (just-informant just)
	  (just-index just)))

;;; ENV 구조체: 환경(environment) = 가정들의 집합 = 하나의 "세계관"
;;;
;;; "이 가정들이 모두 참이라고 가정하는 세계"를 나타낸다.
;;;
;;; 예시:
;;;   E0 = {}          → 빈 환경. 무조건 참인 것들의 기반.
;;;   E1 = {A1}        → "부품M1이 정상"만 가정한 세계
;;;   E4 = {A1, A2}    → "M1 정상 + A1 정상"을 가정한 세계
;;;
;;; assumptions 리스트는 항상 index 오름차순으로 정렬되어 있다.
;;; 이 정렬 덕분에 두 환경이 같은지 equal로 비교할 수 있다.
;;;
;;; (:PREDICATE env?) → (env? x)로 env 구조체인지 판별 가능.
(defstruct (env (:PREDICATE env?)
		(:PRINT-FUNCTION print-env-structure))
	   (index 0)            ; 환경 고유 번호. 예: E0, E1, E2...
	   (count 0)            ; 포함된 가정의 수. 예: {A1,A3} → count=2
	   (assumptions nil)    ; 가정 노드 리스트 (index 오름차순 정렬).
                                ; 예: (A1 A3) where A1.index < A3.index
	   (nodes nil)          ; 이 환경 하에서 참인 노드들의 리스트.
                                ; 예: E1={A1} 하에서 X=6이 참이면, nodes에 X=6 포함.
	   (nogood? nil)        ; nil이면 일관적. non-nil이면 모순.
                                ; 값이 just이면 직접 모순, env이면 그 env의 상위집합이라 모순.
	   (rules nil))         ; 이 환경이 nogood가 될 때 실행할 규칙 리스트.

;;; 환경 출력: "E-번호" 형식. 예: index=3인 환경 → "E-3"
(defun print-env-structure (env stream ignore)
  (declare (ignore ignore))
  (format stream "E-~D" (env-index env)))

;;; 노드의 문자열 표현을 반환. ATMS에 등록된 node-string 함수를 호출한다.
;;; 예: datum이 "X=6"이면 → "X=6" 반환
(defun node-string (node)
  (funcall (atms-node-string (tms-node-atms node)) node))

;;; 디버깅 매크로: atms-debugging이 t일 때만 메시지를 출력한다.
;;; 예: (debugging atms "~%Justifying ~A" node) → "Justifying X=6" 출력
(defmacro debugging (atms msg &optional node &rest args)
  `(when (atms-debugging ,atms)
     (format *trace-output*
	     ,msg (if ,node (node-string ,node)) ,@args)))

;;; 기본 노드→문자열 변환 함수. datum을 그대로 문자열로 변환.
;;; 예: datum = "X=6" → "X=6"
(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

;;; ================================================================
;;; 유틸리티 함수
;;; ================================================================

;;; 정렬된 리스트에 item을 정렬 순서를 유지하며 삽입한다.
;;; 이미 같은 항목(eq)이 있으면 삽입하지 않는다 (중복 방지).
;;; test 함수: item이 현재 원소보다 앞에 와야 하면 t 반환.
;;;
;;; 예: (ordered-insert 3 '(1 5 9) #'<) → (1 3 5 9)
;;;     (ordered-insert 5 '(1 5 9) #'<) → (1 5 9)  ; 이미 존재, 무시
;;;
;;; ATMS에서의 용도: 환경의 assumptions 리스트를 index 순서로 유지.
;;;   (ordered-insert A2 (A1 A3) #'assumption-order) → (A1 A2 A3)
(defun ordered-insert (item list test)
  (cond ((null list) (list item))
	((funcall test item (car list)) (cons item list))
	((eq item (car list)) list)
	(t (cons (car list) (ordered-insert item (cdr list) test)))))

;;; ordered-insert의 매크로 버전. 결과를 list 변수에 바로 대입한다.
;;; 예: (ordered-push new-node my-list #'assumption-order)
;;;     → my-list가 정렬 상태를 유지하며 new-node 포함
(defmacro ordered-push (item list test)
  `(setq ,list (ordered-insert ,item ,list ,test)))

;;; 가정 노드 간 정렬 순서: index가 작은 것이 앞.
;;; 예: A1(index=1) vs A3(index=3) → t (A1이 앞)
;;; 환경의 assumptions 리스트를 일관된 순서로 유지하는 데 사용.
(defun assumption-order (a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))

;;; 환경 간 정렬 순서: index가 작은 것이 앞.
(defun env-order (e1 e2)
  (< (env-index e1) (env-index e2)))


;;; ================================================================
;;; 추론 엔진 인터페이스 (Inference Engine Interface)
;;; ================================================================
;;; 외부 추론 엔진이 ATMS를 사용하기 위해 호출하는 함수들.
;;; 주요 API:
;;;   create-atms     - ATMS 인스턴스 생성
;;;   tms-create-node - 노드(명제) 생성
;;;   justify-node    - 정당화(추론 규칙) 등록 ← 가장 핵심적인 함수
;;;   assume-node     - 기존 노드를 가정으로 변환
;;;   make-contradiction - 노드를 모순으로 표시
;;;   nogood-nodes    - 특정 노드 조합이 동시에 참일 수 없음을 선언
;;; ================================================================

;;; ATMS 인스턴스를 생성한다.
;;; 내부적으로 두 가지를 자동 초기화한다:
;;;   1) contra-node: 모순을 나타내는 더미 노드 (모든 nogood 정당화의 결론이 됨)
;;;   2) empty-env: 빈 환경 {} (아무 가정 없이도 참인 것들의 기반)
;;;
;;; 예:
;;;   (setq *atms* (create-atms "회로 진단"
;;;                  :debugging t
;;;                  :enqueue-procedure #'my-enqueue))
;;;   → #<ATMS: 회로 진단>
(defun create-atms (title &key (node-string 'default-node-string)
		               (debugging NIL)
			       (enqueue-procedure NIL))
  (let ((atms (make-atms :TITLE title
			 :NODE-STRING node-string
			 :DEBUGGING debugging
			 :ENQUEUE-PROCEDURE enqueue-procedure)))
    (setf (atms-contra-node atms)
	  (tms-create-node atms "The contradiction"
			   :CONTRADICTORYP t))
    (setf (atms-empty-env atms) (create-env atms nil))
    atms))

;;; ATMS의 설정을 변경한다. 제공된 키워드 인자만 변경.
;;; 예: (change-atms *atms* :debugging t)  → 디버깅 모드 활성화
(defun change-atms (atms &key node-string
		              enqueue-procedure debugging)
  (if node-string (setf (atms-node-string atms) node-string))
  (if debugging (setf (atms-debugging atms) debugging))
  (if enqueue-procedure
      (setf (atms-enqueue-procedure atms) enqueue-procedure)))

;;; 노드가 "무조건 참"인지 확인한다.
;;; label의 첫 환경이 빈 환경({})이면, 아무 가정 없이도 참 → 무조건 참.
;;;
;;; 예: (justify-node "관측" F=10 '())을 하면 F=10.label = [{}]
;;;     (true-node? F=10) → t   (관측값은 무조건 참)
;;;     (true-node? X=6)  → nil (가정이 필요함)
(defun true-node? (node)
  (eq (car (tms-node-label node))
      (atms-empty-env (tms-node-atms node))))

;;; 노드가 특정 환경 하에서 "참(in)"인지 확인한다.
;;; env가 주어지면: label 중 하나라도 env의 부분집합이면 참.
;;; env가 없으면:   label이 비어있지 않으면 참 (어떤 환경에서든 참).
;;;
;;; 예: X=6.label = [{A1}] 일 때
;;;   (in-node? X=6 {A1,A2}) → t   ({A1} ⊂ {A1,A2} 이므로)
;;;   (in-node? X=6 {A2})   → nil  ({A1} ⊄ {A2})
;;;   (in-node? X=6)         → t   (label이 비어있지 않으므로)
(defun in-node? (n &optional env)
  (if env
      (some #'(lambda (le) (subset-env? le env))
	    (tms-node-label n))
      (not (null (tms-node-label n)))))

;;; in-node?의 반대. 환경 env 하에서 노드가 "참이 아닌지" 확인.
;;; 예: (out-node? X=6 {A2}) → t   (A2만으로는 X=6을 도출할 수 없음)
(defun out-node? (n env) (not (in-node? n env)))

;;; 노드 n이 환경 env와 "일관적으로 양립 가능한지" 확인한다.
;;; n의 label 중 하나라도 env와 합쳤을 때 nogood가 아니면 → 양립 가능.
;;;
;;; 예: X=6.label = [{A1}], env = {A2} 일 때
;;;   union({A1}, {A2}) = {A1,A2}가 nogood가 아니면 → t (양립 가능)
;;;   union({A1}, {A2}) = {A1,A2}가 nogood이면     → nil (모순)
(defun node-consistent-with? (n env)
  (some #'(lambda (le) (not (env-nogood? (union-env le env))))
	(tms-node-label n)))

;;; 새 노드(명제)를 생성한다.
;;;
;;; :assumptionp t    → 가정 노드. 자기 자신만 포함하는 환경이 label에 자동 추가.
;;; :contradictoryp t → 모순 노드. 이 노드의 label에 환경이 추가되면 그 환경은 nogood.
;;;
;;; 예:
;;;   ;; 일반 노드 생성: label이 빈 상태로 시작
;;;   (tms-create-node *atms* "X=6")
;;;   → #<NODE: X=6>, label = []
;;;
;;;   ;; 가정 노드 생성: 자기 자신을 가정하면 참
;;;   (tms-create-node *atms* "M1정상" :assumptionp t)
;;;   → A-1, label = [{A-1}]
;;;
;;;   ;; 모순 노드 생성 (create-atms 내부에서 contra-node 용도)
;;;   (tms-create-node *atms* "모순" :contradictoryp t)
;;;   → label에 환경이 들어오면 그 환경은 자동으로 nogood 처리
(defun tms-create-node (atms datum &key assumptionp contradictoryp
				   &aux node)
  (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
			    :DATUM datum
			    :ASSUMPTION? assumptionp
			    :CONTRADICTORY? contradictoryp
			    :ATMS atms))
  (push node (atms-nodes atms))
  (if contradictoryp (push node (atms-contradictions atms)))
  ;; 가정 노드이면: 자기 자신만 포함한 환경 {자신}을 만들어서 label에 추가.
  ;; 예: A1 생성 시 → A1.label = [{A1}]  "A1을 가정하면 A1은 참"
  (when assumptionp
    (push node (atms-assumptions atms))
    (push (create-env atms (list node)) (tms-node-label node)))
  node)


;;; 기존의 일반 노드를 사후적으로 가정(assumption)으로 변환한다.
;;; 자기 자신을 포함하는 환경을 만들어 label에 추가하고, 후속 정당화들에 전파한다.
;;;
;;; 예: 이미 존재하는 일반 노드 N을 가정으로 변환
;;;   (assume-node N)
;;;   → N.assumption? = t
;;;   → N.label에 {N} 환경 추가
;;;   → N을 전건으로 사용하는 모든 정당화에 update로 전파
(defun assume-node (node &aux atms)
  (unless (tms-node-assumption? node)
    (setq atms (tms-node-atms node))
    (debugging atms  "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t)
    (push node (atms-assumptions atms))
    (update (list (create-env atms (list node)))
	    node
	    'ASSUME-NODE)))

;;; 기존 노드를 사후적으로 모순(contradiction)으로 표시한다.
;;; 이 노드의 label에 있는 모든 환경을 하나씩 꺼내서 nogood로 처리한다.
;;; new-nogood가 환경을 label에서 제거하므로, 반복하다 label이 비면 종료.
;;;
;;; 예: 노드 N.label = [{A1}, {A2,A3}] 일 때 (make-contradiction N) 호출:
;;;   1회차: {A1}을 nogood로 등록 → label에서 {A1} 제거
;;;   2회차: {A2,A3}을 nogood로 등록 → label에서 {A2,A3} 제거
;;;   3회차: label이 비었으므로 종료
;;;   → {A1}과 {A2,A3} 모두 모순 세계관으로 등록됨
(defun make-contradiction
       (node &aux (atms (tms-node-atms node)) nogood)
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (atms-contradictions atms))
    (do nil (nil)
      (if (setq nogood (car (tms-node-label node)))
	  (new-nogood atms nogood 'MAKE-CONTRADICTION)
	  (return nil)))))

;;; ★ ATMS의 가장 핵심 함수 ★
;;; "informant 규칙에 의해, antecedents가 모두 참이면 consequence도 참이다"
;;; 라는 정당화를 등록하고, 레이블 전파를 수행한다.
;;;
;;; 동작 과정:
;;;   1) just 구조체 생성, 양방향 연결 (consequence ← just → antecedents)
;;;   2) propagate 호출: 전건들의 label을 weave로 교차곱 → 결론에 update
;;;
;;; 예: 가정 A1="M1정상"(label=[{A1}]), A2="A1정상"(label=[{A2}])일 때
;;;   (justify-node "덧셈규칙" F=12 (list A2 X=6))
;;;   ;; X=6.label = [{A1}]이면
;;;   ;; weave: {A2} × {A1} = {A1,A2}
;;;   ;; → F=12.label = [{A1,A2}]  "M1과 A1이 정상이면 F=12"
;;;
;;; 예: 전건 없는 정당화 = 무조건 참
;;;   (justify-node "관측" F=10 '())
;;;   ;; weave: 전건 없음 → 빈 환경 {}만 남음
;;;   ;; → F=10.label = [{}]  "무조건 참"
(defun justify-node (informant consequence antecedents &aux just atms)
  (setq atms (tms-node-atms consequence)
	just (make-just :INDEX (incf (atms-just-counter atms))
			:INFORMANT informant
			:CONSEQUENCE consequence
			:ANTECEDENTS antecedents))
  ;; 양방향 연결: consequence.justs에 just 추가, 각 antecedent.consequences에도 추가
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (atms-justs atms))
  (debugging atms
	     "~%Justifying ~A in terms of ~A on ~A"
	     consequence
	     informant
	     (mapcar #'node-string antecedents))
  ;; 빈 환경에서 시작하여 전건들의 label을 결합(weave)한 뒤 결론에 전파(update)
  (propagate just nil (list (atms-empty-env atms)))
  just)

;;; "이 노드들이 동시에 참이면 모순"이라고 선언한다.
;;; 내부적으로는 contra-node(모순 더미 노드)를 결론으로 하는 정당화를 등록한다.
;;;
;;; 예: M1과 M2가 동시에 고장일 수 없다면:
;;;   (nogood-nodes "물리적제약" (list 고장-M1 고장-M2))
;;;   ;; → justify-node("물리적제약", contra-node, (고장-M1, 고장-M2))
;;;   ;; → weave로 환경 결합 → 그 환경이 contra-node에 도달 → nogood 등록
(defun nogood-nodes (informant nodes)
  (justify-node informant
		(atms-contra-node (tms-node-atms (car nodes)))
		nodes))

;;; ================================================================
;;; 레이블 갱신 알고리즘 (Label Updating)
;;; ================================================================
;;; ATMS의 핵심 알고리즘. 정당화가 등록되면 다음 3단계로 레이블을 전파한다:
;;;
;;;   propagate → weave → update → update-label → (재귀적 propagate)
;;;
;;; 전체 흐름 예시:
;;;   justify-node("규칙", C, [A, B]) 호출 시:
;;;   1) weave: A.label × B.label 교차곱 계산 (nogood 제외, 최소성 유지)
;;;   2) update: 결과 환경들을 C.label에 반영
;;;   3) C를 전건으로 하는 다른 정당화들에 재귀적으로 propagate
;;; ================================================================

;;; 정당화의 전건 환경들을 weave로 결합하고, 결과를 결론 노드에 update한다.
;;;
;;; antecedent: 이번에 label이 변경된 특정 전건 (최초 호출 시 nil).
;;;   → weave에서 이 노드는 건너뛴다 (이미 envs에 반영되어 있으므로).
;;; envs: 출발 환경 리스트. 최초 호출 시 [빈환경], 재전파 시 [새 환경들].
;;;
;;; 예: just = (A, B → C), A.label=[{A1}], B.label=[{A2}]
;;;   propagate(just, nil, [{}])
;;;   → weave(nil, [{}], [A, B]) → [{A1,A2}]
;;;   → update([{A1,A2}], C, just)
(defun propagate (just antecedent envs &aux new-envs)
  (if (setq new-envs (weave antecedent envs (just-antecedents just)))
      (update new-envs (just-consequence just) just)))

;;; 새 환경들을 결론 노드의 label에 반영하고, 후속 정당화들에 전파한다.
;;;
;;; 핵심 분기:
;;;   1) consequence가 모순 노드 → 새 환경들을 모두 nogood로 등록하고 종료
;;;   2) consequence가 일반 노드 → update-label로 label 갱신
;;;      → 새로 추가된 환경이 있으면 외부 규칙 실행 + 후속 정당화에 재귀 전파
;;;
;;; 예: consequence = contra-node, new-envs = [{A1,A2}]
;;;   → new-nogood(atms, {A1,A2}, just)  "{A1,A2}는 모순!"
;;;
;;; 예: consequence = X=6, new-envs = [{A1}]
;;;   → X=6.label에 {A1} 추가
;;;   → X=6을 전건으로 사용하는 정당화들에 propagate
(defun update (new-envs consequence just &aux atms enqueuef)
  (setq atms (tms-node-atms consequence))
  ;; 분기1: 결론이 모순 노드이면, 모든 새 환경을 nogood로 등록하고 종료
  (when (tms-node-contradictory? consequence)
    (dolist (env new-envs) (new-nogood atms env just))
    (return-from update nil))
  ;; 분기2: 일반 노드 — label을 갱신하고 실제로 추가된 환경만 남김
  (setq new-envs (update-label consequence new-envs))
  (unless new-envs (return-from update nil))  ; 새로 추가된 것이 없으면 종료
  ;; 외부 추론엔진에 규칙 실행 알림 (label이 변경되었으므로)
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (tms-node-rules consequence))
      (funcall enqueuef rule))
    (setf (tms-node-rules consequence) nil))
  ;; 이 노드를 전건으로 사용하는 모든 정당화에 재귀적으로 전파
  (dolist (supported-just (tms-node-consequences consequence))
    (propagate supported-just consequence new-envs)
  ;; 전파 중 일부 환경이 nogood가 되었을 수 있으므로, label에서 제거된 것은 걸러냄
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (unless (member (car new-envs) (tms-node-label consequence))
      (rplaca new-envs nil)))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (unless new-envs (return-from update nil))))

;;; 노드의 label에 새 환경들을 추가한다. 최소성(minimality)을 유지한다.
;;; 반환값: 실제로 새로 추가된 환경들의 리스트 (기존에 포함된 것은 제외).
;;;
;;; 최소성 규칙 (compare-env 결과에 따른 처리):
;;;   :EQ  → 동일한 환경이 이미 있음 → 새 환경 무시
;;;   :S21 → 새 환경이 기존 환경의 상위집합 → 새 환경 무시 (더 큰 것은 불필요)
;;;   :S12 → 새 환경이 기존 환경의 부분집합 → 기존 환경 제거 (더 작은 것이 우선)
;;;
;;; 예: 기존 label = [{A1,A2}], 새 환경 = {A1}
;;;   compare-env({A1}, {A1,A2}) → :S12 (새 것이 부분집합)
;;;   → {A1,A2} 제거, {A1} 추가
;;;   → label = [{A1}]
;;;
;;; 예: 기존 label = [{A1}], 새 환경 = {A1,A2}
;;;   compare-env({A1,A2}, {A1}) → :S21 (새 것이 상위집합)
;;;   → {A1,A2} 무시
;;;   → label = [{A1}] (변화 없음)
(defun update-label (node new-envs &aux envs)
  (setq envs (tms-node-label node))
  ;; 각 새 환경에 대해, 기존 label의 모든 환경과 비교
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (do ((nenvs envs (cdr nenvs)))
	((null nenvs) (push (car new-envs) envs))  ; 기존에 없으면 추가
      (cond ((null (car nenvs)))      ; 이미 제거된 항목 건너뜀
	    ((null (car new-envs)))   ; 이미 무시된 새 환경 건너뜀
	    ((case (compare-env (car new-envs) (car nenvs))
	       ((:EQ :S21)            ; 새 환경 ⊇ 기존 → 새 환경 버림
		(rplaca new-envs nil))
	       (:S12                  ; 새 환경 ⊂ 기존 → 기존 환경 제거
		(setf (env-nodes (car nenvs))
		      (delete node (env-nodes (car nenvs))
			      :COUNT 1))
		(rplaca nenvs nil)))))))
  ;; nil로 표시한 항목들 정리
  (setq new-envs (delete nil new-envs :TEST #'eq))
  ;; 새로 추가된 환경들에 이 노드를 등록 (env → node 역참조)
  (dolist (new-env new-envs) (push node (env-nodes new-env)))
  (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  new-envs)

;;; ★ ATMS의 핵심 알고리즘: 환경 교차곱(cross-product) ★
;;; 여러 전건(antecedent) 노드의 label 환경들을 결합하여,
;;; "모든 전건이 동시에 참이 되는 최소 환경"들을 계산한다.
;;;
;;; antecedent: 이미 envs에 반영된 노드 (건너뜀). 재전파 시 사용.
;;; envs: 출발 환경 리스트.
;;; antecedents: 전건 노드 리스트.
;;;
;;; 예: A.label = [{A1}, {A2}], B.label = [{A3}] 일 때
;;;   weave(nil, [{}], [A, B])
;;;
;;;   A 처리: {} × {A1} = {A1}, {} × {A2} = {A2}
;;;     envs = [{A1}, {A2}]
;;;
;;;   B 처리: {A1} × {A3} = {A1,A3}, {A2} × {A3} = {A2,A3}
;;;     envs = [{A1,A3}, {A2,A3}]
;;;
;;;   결과: [{A1,A3}, {A2,A3}]
;;;   → "A1+A3 가정하면 A와 B 모두 참" 또는 "A2+A3 가정하면 A와 B 모두 참"
;;;
;;; nogood인 환경은 즉시 제거하고, 상위집합도 제거하여 최소성 유지.
;;; 어떤 전건의 label이 비어있으면 교차곱 결과도 빈 리스트 → nil 반환.
(defun weave (antecedent envs antecedents &aux new-envs new-env)
  (setq envs (copy-list envs))
  (dolist (node antecedents)
    (unless (eq node antecedent)   ; 이미 반영된 antecedent는 건너뜀
      (setq new-envs nil)
      (dolist (env envs)
	(if env
	    (dolist (node-env (tms-node-label node))
	      ;; env와 node-env의 합집합 계산
	      (setq new-env (union-env env node-env))
	      (unless (env-nogood? new-env)  ; nogood이면 버림
		;; 최소성 검사: 이미 있는 것의 상위집합이면 버리고,
		;; 부분집합이면 기존 것을 교체
		(do ((nnew-envs new-envs (cdr nnew-envs)))
		    ((null nnew-envs) (push new-env new-envs))  ; 중복 없으면 추가
		  (when (car nnew-envs)
		    (case (compare-env new-env (car nnew-envs))
		      ((:EQ :S21) (return nil))       ; 이미 같거나 더 큰 것 → 버림
		      (:S12 (rplaca nnew-envs nil))))))))) ; 더 작은 것 → 기존 제거
      (setq envs (delete nil new-envs :TEST #'eq))
      ;; 전건의 label이 비어있어서 교차곱 결과가 비면 → 전체 실패
      (unless envs (return-from weave nil))))
  envs)

;;; 노드 리스트의 모든 노드가 동시에 참일 수 있는 일관된 환경이 존재하는지 확인.
;;; weave의 간소화 버전 (환경을 직접 구성하지 않고 존재 여부만 확인).
;;;
;;; 예: A.label = [{A1}], B.label = [{A2}]
;;;   (in-antecedent? (list A B))
;;;   → {A1} ∪ {A2} = {A1,A2}가 nogood가 아니면 → t
(defun in-antecedent? (nodes)
  (or (null nodes)
      (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))

;;; weave?의 재귀 도우미. 현재까지 결합한 env에 나머지 nodes를 하나씩 결합.
;;; 어떤 경로에서든 모든 노드를 결합할 수 있으면 t 반환.
(defun weave? (env nodes &aux new-env)
  (cond ((null nodes) t)  ; 모든 노드 결합 완료 → 성공
	(t (dolist (e (tms-node-label (car nodes)))
	     (setq new-env (union-env e env))
	     (unless (env-nogood? new-env)
	       (if (weave? new-env (cdr nodes))
		   (return T)))))))

;;; 모든 노드가 주어진 환경 env 하에서 참인지 확인한다.
;;; 예: env = {A1,A2}, nodes = (X=6, Y=6)
;;;   X=6.label에 {A1}⊂{A1,A2} 있고, Y=6.label에 {A2}⊂{A1,A2} 있으면 → t
(defun supporting-antecedent? (nodes env)
  (dolist (node nodes t) (unless (in-node? node env) (return nil))))


;;; 노드를 ATMS에서 제거한다. consequences가 있으면 제거 불가 (에러 발생).
;;; 이 노드를 참조하는 모든 정당화 연결과 환경 연결을 정리한다.
;;;
;;; 예: 더 이상 필요없는 중간 노드 제거
;;;   (remove-node temp-node)
;;;   → atms.nodes에서 제거
;;;   → 이 노드를 전건으로 받는 정당화에서 consequences 역참조 제거
;;;   → label의 각 환경에서 이 노드 제거
(defun remove-node (node &aux atms)
  (if (tms-node-consequences node)
      (error "Can't remove node with consequences"))
  (setq atms (tms-node-atms node))
  (setf (atms-nodes atms)
	(delete node (atms-nodes atms) :test #'eq :count 1))
  ;; 이 노드를 결론으로 하는 정당화들: 각 전건에서 이 정당화에 대한 역참조 제거
  (dolist (just (tms-node-justs node))
    (dolist (ant (just-antecedents just))
      (setf (tms-node-consequences ant)
	    (delete just (tms-node-consequences ant)
		    :test #'eq :count 1))))
  ;; 이 노드가 속한 환경들에서 이 노드 제거
  (dolist (env (tms-node-label node))
    (setf (env-nodes env)
	  (delete node (env-nodes env) :test #'eq :count 1))))

;;; ================================================================
;;; 환경 생성 및 확장 (Creating and Extending Environments)
;;; ================================================================
;;; 환경은 가정 집합의 고유 표현. 같은 가정 집합의 환경은 하나만 존재한다.
;;; lookup-env로 기존 환경을 찾고, 없으면 create-env로 새로 생성한다.
;;; ================================================================

;;; 새 환경을 생성한다. env-table에 등록하고, 기존 nogood의 상위집합인지 검사한다.
;;;
;;; 예: (create-env *atms* (list A1 A3))
;;;   → E5 = {A1, A3}, count=2
;;;   → env-table의 count=2 버킷에 등록
;;;   → nogood-table 확인: {A1}이 nogood이면 E5도 자동으로 nogood
(defun create-env (atms assumptions &aux e)
  (setq e (make-env :INDEX (incf (atms-env-counter atms))
		    :ASSUMPTIONS assumptions
		    :COUNT (length assumptions)))
  (setf (atms-env-table atms)
	(insert-in-table (atms-env-table atms) e))
  (set-env-contradictory atms e)  ; 기존 nogood의 상위집합인지 확인
  e)

;;; 두 환경의 합집합을 구한다.
;;; 작은 쪽의 가정을 하나씩 큰 쪽에 cons-env로 추가한다.
;;; 중간에 nogood가 되면 즉시 nil 반환 (실패).
;;;
;;; 예: E1={A1}, E3={A3}
;;;   union-env(E1, E3)
;;;   → A1을 {A3}에 추가 → cons-env(A1, E3) → {A1,A3} = E5
;;;   → E5가 nogood가 아니면 → E5 반환
;;;
;;; 최적화: 작은 쪽을 e1으로 놓아서 cons-env 호출 횟수를 줄임 (psetq로 교환).
(defun union-env (e1 e2)
  (when (> (env-count e1)
	   (env-count e2))
    (psetq e1 e2 e2 e1))  ; e1이 항상 작은 쪽
  (dolist (assume (env-assumptions e1))
    (setq e2 (cons-env assume e2))
    (if (env-nogood? e2) (return nil)))  ; 중간에 nogood → 실패
  e2)

;;; 가정 하나를 기존 환경에 추가하여 새 환경을 구한다.
;;; 이미 존재하는 환경이면 lookup-env로 재사용 (구조적 공유).
;;;
;;; 예: assumption=A2, env={A1,A3}
;;;   nassumes = ordered-insert(A2, (A1 A3)) → (A1 A2 A3)
;;;   lookup-env((A1 A2 A3)) → 이미 있으면 그 환경 반환
;;;                          → 없으면 create-env로 새로 생성
(defun cons-env (assumption env &aux nassumes)
  (setq nassumes (ordered-insert assumption
				 (env-assumptions env)
				 #'assumption-order))
  (or (lookup-env nassumes)
      (create-env (tms-node-atms assumption) nassumes)))

;;; 가정 리스트로 환경을 찾거나 새로 만든다.
;;; assumptions가 nil이면 빈 환경 반환.
;;; 예: (find-or-make-env (list A1 A3) *atms*) → {A1,A3} 환경
(defun find-or-make-env (assumptions atms)
  (unless assumptions
    (return-from find-or-make-env (atms-empty-env atms)))
  ;; 가정 리스트는 이미 정렬되어 있다고 가정함
  (or (lookup-env assumptions)
      (create-env atms assumptions)))

;;; ================================================================
;;; 환경 테이블 (Env Tables)
;;; ================================================================
;;; 환경을 가정 개수(count)별로 분류하여 빠르게 검색한다.
;;; 테이블 구조: ((count1 env1a env1b ...) (count2 env2a ...) ...)
;;; count 오름차순으로 정렬되어 있다.
;;;
;;; 예: ((0 E0) (1 E1 E2 E3) (2 E4 E5) (3 E7))
;;;   → count=0: 빈 환경 1개, count=1: 가정 1개짜리 3개, ...
;;; ================================================================

;;; 환경을 테이블에 삽입한다.
;;; 같은 count의 버킷이 이미 있으면 그 버킷에 추가, 없으면 새 버킷 생성.
;;;
;;; 예: count=2 환경 E5를 삽입할 때
;;;   기존 테이블: ((0 E0) (1 E1 E2) (3 E7))
;;;   → count=2 버킷이 없으므로 새 버킷 생성
;;;   → 결과: ((0 E0) (1 E1 E2) (2 E5) (3 E7))
(defun insert-in-table (table env &aux count entry)
  (setq count (env-count env)
	entry (assoc count table :TEST #'=))
  (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
	(t (ordered-insert
	     (list count env) table
	     #'(lambda (entry1 entry2)
		 (< (car entry1) (car entry2)))))))

;;; 가정 리스트와 동일한 환경을 테이블에서 찾는다.
;;; 같은 크기(count) 버킷에서 선형 탐색. 없으면 nil.
;;;
;;; 예: assumes = (A1 A3)
;;;   → env-table에서 count=2 버킷을 찾음
;;;   → 그 버킷의 환경들 중 assumptions가 (A1 A3)인 것을 equal로 비교
;;;   → 찾으면 그 환경 반환, 없으면 nil
(defun lookup-env (assumes)
  (dolist (env (cdr (assoc (length assumes)
			   (atms-env-table (tms-node-atms (car assumes)))
			   :TEST #'=))
	       nil)
    (if (equal (env-assumptions env) assumes)
	(return env))))

;;; e1이 e2의 부분집합(또는 동일)인지 확인한다.
;;; 예: {A1} ⊂ {A1,A2} → t
;;;     {A1,A2} ⊂ {A1} → nil (e1이 더 크면 불가)
;;;     {A1} ⊂ {A1}    → t  (eq로 동일 객체 → 즉시 t)
(defun subset-env? (e1 e2)
  (cond ((eq e1 e2) t)                   ; 동일 객체 → 확실히 부분집합
	((> (env-count e1)
	    (env-count e2)) nil)          ; e1이 더 크면 부분집합 불가
	((subsetp (env-assumptions e1)
		  (env-assumptions e2)))))

;;; 두 환경의 부분집합 관계를 비교한다.
;;; 반환값:
;;;   :EQ  → 동일 (eq)
;;;   :S12 → e1 ⊂ e2 (e1이 진부분집합)
;;;   :S21 → e2 ⊂ e1 (e2가 진부분집합)
;;;   nil  → 비교 불가 (어느 쪽도 부분집합이 아님)
;;;
;;; 예: compare-env({A1}, {A1,A2})   → :S12
;;;     compare-env({A1,A2}, {A1})   → :S21
;;;     compare-env({A1}, {A2})      → nil
;;;     compare-env({A1}, {A1})      → :EQ (같은 객체일 때)
(defun compare-env (e1 e2)
  (cond ((eq e1 e2) :EQ)
	((< (env-count e1) (env-count e2))
	 (if (subsetp (env-assumptions e1)
		      (env-assumptions e2))
	     :S12))
	((subsetp (env-assumptions e2) (env-assumptions e1))
	 :S21)))

;;; ================================================================
;;; Nogood 처리 (Processing Nogoods)
;;; ================================================================
;;; Nogood = "이 가정 조합은 모순이므로 동시에 성립할 수 없다"
;;;
;;; 예: {A1,A2}가 nogood → "M1정상과 A1정상을 동시에 가정하면 모순"
;;;
;;; nogood가 등록되면:
;;;   1) 해당 환경을 모든 노드의 label에서 제거
;;;   2) nogood-table에 등록
;;;   3) 이 환경의 상위집합인 모든 환경도 nogood로 전파
;;; ================================================================

;;; 환경 cenv를 nogood(모순)으로 등록한다.
;;;
;;; 동작 과정:
;;;   1) cenv.nogood? = just 로 표시
;;;   2) cenv가 포함된 모든 노드의 label에서 cenv 제거
;;;   3) nogood-table에 등록
;;;   4) nogood-table에서 cenv보다 큰 기존 nogood 중 cenv의 상위집합은 제거
;;;      (더 작은 nogood가 있으면 큰 nogood는 불필요)
;;;   5) env-table에서 cenv의 상위집합인 환경들도 전부 nogood로 표시
;;;
;;; 예: cenv = {A1} (count=1) 이 nogood로 등록되면:
;;;   → {A1,A2}, {A1,A3}, {A1,A2,A3} 등 모든 상위집합도 nogood
;;;   → nogood-table에 {A1,A2}같은 기존 큰 nogood는 제거 (최소만 유지)
(defun new-nogood (atms cenv just &aux count)
  (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
  (setf (env-nogood? cenv) just)
  (remove-env-from-labels cenv atms)
  (setf (atms-nogood-table atms)
	(insert-in-table (atms-nogood-table atms) cenv))
  (setq count (env-count cenv))
  ;; nogood-table에서: cenv보다 큰 기존 nogood 중 cenv의 상위집합을 제거
  ;; (더 작은 nogood가 있으므로 큰 nogood는 중복)
  (dolist (entry (atms-nogood-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(if (subset-env? cenv old)
	    (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
  ;; env-table에서: cenv보다 큰 환경 중 cenv의 상위집합을 전부 nogood로 표시
  (dolist (entry (atms-env-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(when (and (not (env-nogood? old))
		   (subset-env? cenv old))
	  (setf (env-nogood? old) cenv)
	  (remove-env-from-labels old atms))))))

;;; 새로 생성된 환경이 기존 nogood의 상위집합인지 검사한다.
;;; create-env에서 호출. 이미 nogood인 환경의 상위집합이면 자동으로 nogood 표시.
;;;
;;; 예: nogood-table에 {A1}이 있고, 새 환경 {A1,A2}를 생성하면
;;;   → {A1} ⊂ {A1,A2} → {A1,A2}.nogood? = {A1}
;;;
;;; 최적화: nogood-table이 count 오름차순이므로,
;;;   새 환경보다 큰 nogood는 부분집합이 될 수 없어 조기 종료.
(defun set-env-contradictory (atms env &aux count)
  (cond ((env-nogood? env) t)  ; 이미 nogood이면 무시
	(t (setq count (env-count env))
	   (dolist (entry (atms-nogood-table atms))
	     (cond ((> (car entry) count)
		    (return nil))  ; 더 큰 nogood는 부분집합이 될 수 없음 → 종료
		   (t (dolist (cenv (cdr entry))
			(when (subset-env? cenv env)
			  (setf (env-nogood? env)
				cenv)
			  (return t)))))))))

;;; 환경을 그 환경이 속한 모든 노드의 label에서 제거한다.
;;; nogood가 된 환경은 더 이상 유효한 세계관이 아니므로 label에 있으면 안 된다.
;;;
;;; 예: env = {A1,A2}가 nogood 되면
;;;   → {A1,A2}를 label에 가진 모든 노드에서 이 환경 제거
;;;   → 해당 노드들의 label이 변경됨
;;;   → 외부 규칙이 있으면 enqueue로 알림
(defun remove-env-from-labels (env atms &aux enqueuef)
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (env-rules env))
      (funcall enqueuef rule))
    (setf (env-rules env) nil))
  (dolist (node (env-nodes env))
    (setf (tms-node-label node)
	  (delete env (tms-node-label node) :COUNT 1))))

;;; ================================================================
;;; 해석 구성 (Interpretation Construction)
;;; ================================================================
;;; 여러 선택지(choice-set) 중에서 하나씩 골라, 모순 없이 양립 가능한
;;; 일관된 해석(interpretation)을 찾는다. 깊이우선 탐색 사용.
;;;
;;; 예: 진단 문제에서
;;;   choice-set 1: {M1정상, M1고장} 중 하나
;;;   choice-set 2: {A1정상, A1고장} 중 하나
;;;   → (M1정상+A1고장), (M1고장+A1정상) 등 일관된 조합을 찾음
;;; ================================================================

(proclaim '(special *solutions*))

;;; 일관된 해석(interpretation)들을 모두 찾는다.
;;;
;;; choice-sets: 선택지 그룹의 리스트. 각 그룹에서 하나씩 선택해야 함.
;;;   예: ((M1정상 M1고장) (A1정상 A1고장))
;;; defaults: 가능하면 추가할 기본 가정 리스트.
;;;
;;; 동작:
;;;   1) 각 choice-set의 노드들의 label을 모아서 환경 리스트로 변환
;;;   2) 깊이우선 탐색으로 각 그룹에서 하나씩 골라 union
;;;   3) nogood가 아닌 최소 환경들만 *solutions*에 수집
;;;   4) defaults가 있으면 추가 확장 시도
;;;
;;; 예: choice-sets = ((A B) (C D))
;;;   A.label=[{A1}], B.label=[{A2}], C.label=[{A3}], D.label=[{A4}]
;;;   → {A1}∪{A3}, {A1}∪{A4}, {A2}∪{A3}, {A2}∪{A4} 중 nogood 아닌 것 수집
(defun interpretations (atms choice-sets
			&optional defaults &aux solutions)
  (if (atms-debugging atms)
   (format *trace-output*
	   "~% Constructing interpretations depth-first..."))
  (let ((*solutions* nil)
	;; 각 choice-set의 노드들을 환경 리스트로 변환
	;; 예: (A B) → A.label ++ B.label = ({A1} {A2})
	(choice-sets
	  (mapcar #'(lambda (alt-set)
		      (mapcan #'(lambda (alt)
				  (copy-list (tms-node-label alt)))
			      alt-set))
		  choice-sets)))
    ;; 첫 번째 choice-set의 각 환경에서 시작하여 깊이우선 탐색
    (dolist (choice (car choice-sets))
      (get-depth-solutions1 choice (cdr choice-sets)))
    (setq *solutions* (delete nil *solutions* :TEST #'eq))
    (unless *solutions*
      (if choice-sets (return-from interpretations nil)
	              (setq *solutions* (list (atms-empty-env atms)))))
    ;; 기본 가정(defaults) 확장
    (when defaults
      (setq solutions *solutions* *solutions* nil)
      (dolist (solution solutions)
	(extend-via-defaults solution defaults defaults)))
    (delete nil *solutions* :TEST #'eq)))

;;; 깊이우선 탐색으로 일관된 해석을 구성한다.
;;;
;;; solution: 현재까지 결합한 환경.
;;; choice-sets: 아직 선택하지 않은 나머지 선택지 그룹들.
;;;
;;; 종료 조건: choice-sets가 비면 → solution이 최소인지 확인 후 *solutions*에 추가.
;;; 가지치기: solution이 nogood이면 즉시 중단.
;;;
;;; 예: solution = {A1}, choice-sets = (({A3} {A4}))
;;;   {A1}∪{A3} = {A1,A3} → nogood? → 아니면 재귀
;;;   {A1}∪{A4} = {A1,A4} → nogood? → 아니면 재귀
(defun get-depth-solutions1 (solution choice-sets
				      &aux new-solution)
  (cond ((null choice-sets)
	 ;; 모든 선택지를 결합 완료 → 최소성 검사 후 추가
	 (unless (do ((old-solutions *solutions* (cdr old-solutions)))
		     ((null old-solutions))
		   (when (car old-solutions)
		     (case (compare-env (car old-solutions) solution)
		       ((:EQ :S12) (return t))    ; 기존이 같거나 더 작으면 추가 안함
		       (:S21 (rplaca old-solutions nil))))) ; 기존이 더 크면 기존 제거
	   (push solution *solutions*)))
	((env-nogood? solution))  ; 현재 solution이 모순이면 가지치기
	(t (dolist (choice (car choice-sets))
	     (setq new-solution (union-env solution choice))
	     (unless (env-nogood? new-solution)
	       (get-depth-solutions1 new-solution
				     (cdr choice-sets)))))))


;;; 기본 가정(defaults)을 해석에 추가 확장한다.
;;; 모순이 생기지 않는 한 가능한 많은 default를 포함시킨다.
;;;
;;; 예: solution = {A1}, defaults = (D1 D2 D3)
;;;   D1 추가: {A1,D1} → nogood 아니면 계속, nogood이면 D1 건너뜀
;;;   D2 추가: {A1,D1,D2} → ...
;;;   모든 default 시도 완료 → 결과가 최대한 확장된 해석
(defun extend-via-defaults (solution remaining original)
  (do ((new-solution)
       (defaults remaining (cdr defaults)))
      ((null defaults)
       ;; 모든 default 시도 완료. 이미 존재하는 솔루션이 아니고,
       ;; 추가 안 된 default가 추가 가능하지 않은 경우에만 *solutions*에 추가
       (or (member solution *solutions* :TEST #'eq)
	   (dolist (default original)
	     (or (member default (env-assumptions solution)
			 :TEST #'eq)
		 (env-nogood? (cons-env default solution))
		 (return t)))
	   (push solution *solutions*)))
    ;; 각 default를 추가 시도
    (setq new-solution (cons-env (car defaults) solution))
    (unless (env-nogood? new-solution)
      (extend-via-defaults new-solution (cdr defaults) original))))

;;; ================================================================
;;; 설명 생성 (Generating Explanations)
;;; ================================================================
;;; 특정 환경 하에서 노드가 왜 참인지를 정당화 DAG로 설명한다.
;;; 결과는 정당화 리스트로, 가정에서 시작하여 목표 노드까지의 추론 경로를 나타냄.
;;; ================================================================

;;; 환경 env 하에서 node가 왜 참인지 설명을 생성한다.
;;; 반환값: 정당화들의 리스트 (DAG 형태의 추론 경로)
;;;
;;; 예: env = {A1}, node = X=6
;;;   → ((ASSUME . A1) <곱셈규칙 1>)
;;;   → "A1을 가정하고, 곱셈규칙에 의해 X=6이 도출됨"
(defun explain-node (node env) (explain-node-1 env node nil nil))

;;; explain-node의 재귀 도우미.
;;; queued-nodes: 순환 방지용. 이미 방문한 노드 리스트.
;;; explanation: 지금까지 수집한 정당화 리스트.
;;;
;;; 4가지 경우:
;;;   1) 이미 방문한 노드 → nil (순환 방지)
;;;   2) 가정 노드이고 env에 포함 → (ASSUME . node) 추가
;;;   3) 이미 explanation에 설명이 있는 노드 → 기존 explanation 반환
;;;   4) 일반 노드 → 정당화를 찾아 전건들을 재귀적으로 설명
(defun explain-node-1 (env node queued-nodes explanation)
  (cond ((member node queued-nodes) nil)  ; 순환 방지
	;; 가정 노드이고 현재 환경에 포함 → 추론의 출발점
	((and (tms-node-assumption? node)
	      (member node (env-assumptions env)))
	 (cons (cons 'ASSUME node) explanation))
	;; 이미 설명된 노드 → 기존 explanation 반환
	((dolist (just explanation)
	   (if (if (listp just)
		   (eq (cdr just) node) (eq (just-consequence just) node))
	       (return explanation))))
	;; 일반 노드 → 정당화를 찾아 전건들을 재귀 설명
	(t (setq queued-nodes (cons node queued-nodes))
	   (dolist (just (tms-node-justs node))
	     ;; 이 정당화의 모든 전건이 env 하에서 참인지 확인
	     (unless (dolist (a (just-antecedents just))
		       (unless (in-node? a env) (return t)))
	      ;; 모든 전건이 참 → 각 전건을 재귀적으로 설명
	      (let ((new-explanation explanation))
		(dolist (a (just-antecedents just)
			   ;; 모든 전건 설명 성공 → 이 정당화를 추가하고 반환
			   (return-from explain-node-1 (cons just new-explanation)))
		  (setq new-explanation
			(explain-node-1 env a queued-nodes new-explanation))
		  (unless new-explanation (return nil)))))))))

;;; ================================================================
;;; 출력 함수 (Printing)
;;; ================================================================
;;; 디버깅과 검사를 위한 출력 함수들.
;;; ================================================================

;;; 노드의 datum과 label(환경 리스트)을 출력한다.
;;; 예: X=6.label = [{A1}, {A2}] → "<X=6,{{A1},{A2}}>"
(defun why-node (node &optional (stream t) (prefix ""))
  (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  (dolist (e (tms-node-label node))
    (env-string e stream))
  (format stream "}>"))

;;; ATMS의 모든 노드를 출력한다.
;;; 예: (why-nodes *atms*) → 각 노드의 datum과 label을 순서대로 출력
(defun why-nodes (atms &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))

;;; 노드의 모든 정당화(이 노드를 지지하는 규칙들)를 출력한다.
;;; 예: (node-justifications X=6)
;;;   → "For X=6:"
;;;   →   "곱셈규칙, <A=3,{...}> <B=2,{...}>"
(defun node-justifications (node &optional (stream t))
  (format t "~% For ~A:" (node-string node))
  (dolist (j (tms-node-justs node))
    (print-justification j stream)))

;;; 정당화 하나를 출력한다: informant 이름과 각 전건 노드의 정보.
(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))

;;; 환경 번호(index)로 환경을 검색한다. 디버깅용 편의 함수.
;;; 예: (e *atms* 5) → E-5 환경 객체 반환
(defun e (atms n)
  (dolist (bucket (atms-env-table atms))
    (dolist (env (cdr bucket))
    (if (= (env-index env) n) (return-from e env)))))

;;; 환경을 출력한다. nogood이면 "*" 표시.
;;; 예: E-5: {A1,A3}     (일관적인 환경)
;;;     E-4:* {A1,A2}    (nogood 환경)
(defun print-env (e &optional (stream t))
  (format stream "~%~A:~A"
	  e (if (env-nogood? e)
		"* " " "))
  (env-string e stream))

;;; 환경의 가정들을 "{A1,A3}" 형식의 문자열로 출력한다.
;;; 가정들의 문자열 표현을 알파벳순으로 정렬하여 출력.
(defun env-string (e &optional stream
                     &aux assumptions strings printer)
  (setq assumptions (env-assumptions e))
  (when assumptions
    (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
  (dolist (a assumptions) (push (funcall printer a) strings))
  (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))

;;; ================================================================
;;; 전체 데이터 출력 (Printing Global Data)
;;; ================================================================

;;; 모든 nogood 환경을 출력한다.
;;; 예: (print-nogoods *atms*)
;;;   → E-4:* {A1,A2}
;;;   → E-7:* {A1,A2,A3}
(defun print-nogoods (atms &optional (stream t))
  (print-env-table (atms-nogood-table atms) stream))

;;; 모든 환경을 출력한다 (nogood 포함).
(defun print-envs (atms &optional (stream t))
  (print-env-table (atms-env-table atms) stream))

;;; 환경 테이블의 모든 환경을 순회하며 출력한다.
(defun print-env-table (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))

;;; ATMS 통계를 출력한다: 환경 테이블과 nogood 테이블의 크기별 항목 수.
;;; 예: (print-atms-statistics *atms*)
;;;   →  For env table:
;;;   →    Length 0, 1    (빈 환경 1개)
;;;   →    Length 1, 4    (가정 1개짜리 환경 4개)
;;;   →    Length 2, 6    (가정 2개짜리 환경 6개)
;;;   →  For nogood table:
;;;   →    Length 2, 1    (가정 2개짜리 nogood 1개)
(defun print-atms-statistics (atms)
  (print-table "~% For env table:" (atms-env-table atms))
  (print-table "~% For nogood table:" (atms-nogood-table atms)))

;;; 테이블의 각 버킷별 크기와 항목 수를 출력한다.
(defun print-table (msg table)
  (format t msg)
  (dolist (entry table)
    (format t "~%   Length ~D, ~D" (car entry)
	    (length (cdr entry)))))
