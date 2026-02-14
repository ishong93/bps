;-*- Mode:  LISP; Syntax: Common-lisp; Package: USER -*-

;;; Justification-based Truth Maintenence System (JTMS)
;;; Version 176.
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;;; JTMS 개요
;;;
;;; JTMS는 논리적 추론 시스템에서 믿음(beliefs)을 관리하는 진리 유지 시스템입니다.
;;;
;;; 주요 개념:
;;; - 노드(Node): 명제나 사실을 표현. IN(믿어짐) 또는 OUT(믿어지지 않음) 상태를 가짐
;;; - 정당화(Justification): "선행조건들이 모두 참이면 결론도 참"이라는 추론 규칙
;;; - 가정(Assumption): 다른 근거 없이 참으로 가정할 수 있는 노드
;;; - 모순(Contradiction): IN이 되면 안 되는 노드 (모순 감지용)
;;;
;;; 사용 예:
;;; 1. JTMS 생성: (setq *jtms* (create-jtms "My TMS"))
;;; 2. 노드 생성: (setq n1 (tms-create-node *jtms* '(sky blue)))
;;;              (setq n2 (tms-create-node *jtms* '(grass green)))
;;;              (setq n3 (tms-create-node *jtms* '(day time)))
;;; 3. 정당화 추가: (justify-node 'rule-1 n3 (list n1 n2))
;;;                ; n1과 n2가 모두 IN이면 n3도 IN이 됨
;;; 4. 가정 활성화: (assume-node n1)  ; n1을 IN으로 만듦
;;;                (assume-node n2)  ; n2를 IN으로 만듦
;;;                ; 이제 n3도 자동으로 IN이 됨
;;; 5. 상태 확인: (why-node n3)  ; n3가 왜 IN인지 출력
;;; 6. 가정 철회: (retract-assumption n1)  ; n1을 OUT으로 만듦
;;;              ; n3도 자동으로 OUT이 됨 (n2만으로는 불충분)

(in-package :COMMON-LISP-USER)

;;; JTMS 주 구조체
;;; title: JTMS의 이름
;;; node-counter: 노드 고유 번호 생성기
;;; just-counter: 정당화 고유 번호 생성기
;;; nodes: 모든 TMS 노드들의 리스트
;;; justs: 모든 정당화들의 리스트
;;; debugging: 디버깅 출력 활성화 플래그
;;; contradictions: 모순 노드들의 리스트
;;; assumptions: 가정 노드들의 리스트
;;; checking-contradictions: 모순 검사 활성화 플래그
;;; node-string: 노드를 문자열로 변환하는 함수
;;; contradiction-handler: 모순 발생 시 호출될 함수
;;; enqueue-procedure: 규칙을 큐에 넣는 함수
(defstruct (jtms (:PRINT-FUNCTION print-jtms))
  (title nil)
  (node-counter 0)             ;; unique namer for nodes.
  (just-counter 0)             ;; unique namer for justifications.
  (nodes nil)                  ;; list of all tms nodes.
  (justs nil)                  ;; list of all justifications
  (debugging nil)              ;; debugging flag
  (contradictions nil)         ;; list of contradiction nodes.
  (assumptions nil)            ;; list of assumption nodes.
  (checking-contradictions T)  ;; For external systems
  (node-string nil)
  (contradiction-handler nil)
  (enqueue-procedure nil))

;;; JTMS 객체를 사람이 읽을 수 있는 형식으로 출력
;;; 예: #<JTMS: My-TMS>
(defun print-jtms (jtms stream ignore)
  (declare (ignore ignore))
  (format stream "#<JTMS: ~A>" (jtms-title jtms)))

;;; TMS 노드 구조체
;;; index: 노드의 고유 번호
;;; datum: 외부 문제 해결자에 대한 포인터 (노드가 표현하는 명제/사실)
;;; label: :IN (믿어짐) 또는 :OUT (믿어지지 않음)
;;; support: 현재 정당화 또는 전제 마커 (:ENABLED-ASSUMPTION 또는 just 구조체)
;;; justs: 이 노드에 대한 가능한 정당화들의 리스트
;;; consequences: 이 노드를 선행조건으로 사용하는 정당화들
;;; mark: 순회 알고리즘을 위한 마커
;;; contradictory?: 모순 노드임을 표시하는 플래그
;;; assumption?: 가정 노드임을 표시하는 플래그
;;; in-rules: 노드가 IN이 될 때 실행될 규칙들
;;; out-rules: 노드가 OUT이 될 때 실행될 규칙들
;;; jtms: 이 노드가 속한 JTMS
(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)
  (datum nil)           ;; pointer to external problem solver
  (label :OUT)          ;; :IN means believed, :OUT means disbelieved
  (support nil)         ;; Current justification or premise marker
  (justs nil)           ;; Possible justifications
  (consequences nil)    ;; Justifications in which it is an antecedent
  (mark nil)            ;; Marker for sweep algorithms
  (contradictory? nil)  ;; Flag marking it as contradictory
  (assumption? nil)     ;; Flag marking it as an assumption.
  (in-rules nil)	;; Rules that should be triggered when node goes in
  (out-rules nil)	;; Rules that should be triggered when node goes out
  (jtms nil))           ;; The JTMS in which this node appears.

;;; TMS 노드를 사람이 읽을 수 있는 형식으로 출력
;;; 예: #<Node: (temperature hot)>
(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (format stream "#<Node: ~A>" (node-string node)))

;;; 정당화(Justification) 구조체
;;; index: 정당화의 고유 번호
;;; informant: 정당화의 출처 (규칙 이름, 추론 엔진 정보 등)
;;; consequence: 이 정당화가 지지하는 결론 노드
;;; antecedents: 결론을 뒷받침하는 선행조건 노드들의 리스트
;;;              (모든 선행조건이 IN이면 결론도 IN이 됨)
(defstruct (just (:PRINT-FUNCTION print-just))
  (index 0)
  informant
  consequence
  antecedents)

;;; 정당화(justification) 객체를 사람이 읽을 수 있는 형식으로 출력
;;; 예: #<Just 5>
(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "#<Just ~D>" (just-index just)))

;;; 노드가 전제(premise)인지 확인
;;; 전제는 선행조건(antecedents)이 없는 정당화로 지원되는 노드
;;; 예: (tms-node-premise? my-node) => T 또는 NIL
(defun tms-node-premise? (node &aux support)
  (and (setq support (tms-node-support node))
       (not (eq support :ENABLED-ASSUMPTION))
       (null (just-antecedents support))))

;;; Simple utilities:

;;; 노드를 문자열로 변환
;;; JTMS에 등록된 node-string 함수를 사용하여 노드를 표현
;;; 예: (node-string my-node) => "(temperature hot)"
(defun node-string (node)
  (funcall (jtms-node-string (tms-node-jtms node)) node))

;;; 디버깅 메시지 출력 매크로
;;; JTMS의 debugging 플래그가 켜져 있을 때만 메시지 출력
;;; 예: (debugging-jtms my-jtms "Processing ~A" my-node)
(defmacro debugging-jtms (jtms msg &optional node &rest args)
  `(when (jtms-debugging ,jtms)
     (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

;;; TMS 에러를 발생시킴
;;; 예: (tms-error "Node ~A is invalid" bad-node)
(defun tms-error (string node) (error string (node-string node)))

;;; 기본 노드 문자열 변환 함수
;;; 노드의 datum 필드를 문자열로 변환
;;; 예: (default-node-string node) => "(is-hot coffee)"
(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

;;; 새로운 JTMS 인스턴스 생성
;;; title: JTMS의 이름
;;; node-string: 노드를 문자열로 변환하는 함수 (기본값: default-node-string)
;;; debugging: 디버깅 모드 활성화 여부
;;; checking-contradictions: 모순 검사 활성화 여부
;;; contradiction-handler: 모순 처리 함수
;;; enqueue-procedure: 규칙을 큐에 넣는 함수
;;; 예: (create-jtms "My TMS" :debugging t)
(defun create-jtms (title &key (node-string 'default-node-string)
                               debugging
                               (checking-contradictions t)
                               (contradiction-handler 'ask-user-handler)
                               enqueue-procedure)
  (make-jtms :TITLE title
	     :NODE-STRING node-string
	     :DEBUGGING debugging
	     :CHECKING-CONTRADICTIONS checking-contradictions
	     :CONTRADICTION-HANDLER contradiction-handler
	     :ENQUEUE-PROCEDURE enqueue-procedure
	     ))
	     
;;; 기존 JTMS의 설정 변경
;;; 제공된 키워드 인자에 해당하는 설정만 업데이트
;;; 예: (change-jtms my-jtms :debugging t :checking-contradictions nil)
(defun change-jtms (jtms &key contradiction-handler node-string
		              enqueue-procedure debugging
                              checking-contradictions)
  (if node-string (setf (jtms-node-string jtms) node-string))
  (if debugging (setf (jtms-debugging jtms) debugging))
  (if checking-contradictions
      (setf (jtms-checking-contradictions jtms)
	    checking-contradictions))
  (if contradiction-handler
      (setf (jtms-contradiction-handler jtms) contradiction-handler))
  (if enqueue-procedure
      (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))

;;; Basic inference-engine interface.

;;; 노드가 믿어지는 상태(IN)인지 확인
;;; 예: (in-node? my-node) => T 또는 NIL
(defun in-node? (node) (eq (tms-node-label node) :IN))

;;; 노드가 믿어지지 않는 상태(OUT)인지 확인
;;; 예: (out-node? my-node) => T 또는 NIL
(defun out-node? (node) (eq (tms-node-label node) :OUT))

;;; JTMS에 새로운 노드 생성
;;; datum: 노드가 표현하는 명제나 사실
;;; assumptionp: 가정(assumption) 노드로 생성할지 여부
;;; contradictoryp: 모순(contradiction) 노드로 생성할지 여부
;;; 예: (tms-create-node my-jtms '(temperature hot) :assumptionp t)
;;;     => #<Node: (temperature hot)>
(defun tms-create-node (jtms datum &key assumptionp contradictoryp)
  (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
			     :DATUM datum
			     :ASSUMPTION? assumptionp
			     :CONTRADICTORY? contradictoryp
			     :JTMS jtms)))
    (if assumptionp (push node (jtms-assumptions jtms)))
    (if contradictoryp (push node (jtms-contradictions jtms)))
    (push node (jtms-nodes jtms))
    node))

;;; 일반 노드를 가정(assumption)으로 변환하고 활성화
;;; 노드가 이미 가정이 아니면 가정으로 만들고, 가정을 활성화함
;;; 예: (assume-node my-node)
;;;     => my-node가 가정으로 변환되고 IN 상태가 됨
(defun assume-node (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node)
    (debugging-jtms jtms "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t))
  (enable-assumption node))

;;; 노드를 모순(contradiction) 노드로 만듦
;;; 이 노드가 IN 상태가 되면 모순이 발생한 것으로 간주
;;; 예: (make-contradiction my-node)
;;;     => my-node가 모순 노드가 되고, IN이면 모순 처리가 시작됨
(defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (jtms-contradictions jtms))
    (check-for-contradictions jtms)))

;;; 노드에 정당화(justification) 추가
;;; informant: 정당화의 출처나 규칙 이름
;;; consequence: 정당화되는 결론 노드
;;; antecedents: 결론을 뒷받침하는 선행조건 노드들의 리스트
;;; 모든 선행조건이 IN이면 결론도 IN이 됨
;;; 예: (justify-node 'modus-ponens conclusion-node (list premise1 premise2))
;;;     => premise1과 premise2가 모두 IN이면 conclusion-node도 IN이 됨
(defun justify-node (informant consequence antecedents &aux just jtms)
  (setq jtms (tms-node-jtms consequence)
	just (make-just :INDEX (incf (jtms-just-counter jtms))
			:INFORMANT informant
			:CONSEQUENCE consequence
			:ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (jtms-justs jtms))
  (debugging-jtms jtms
		  "~%Justifying ~A by ~A using ~A."
		  consequence
		  informant
		  (mapcar #'node-string antecedents))
  (if (or antecedents (out-node? consequence))
      (if (check-justification just) (install-support consequence just))
      (setf (tms-node-support consequence) just))
  (check-for-contradictions jtms))

;;;; Support for adding justifications

;;; 정당화가 설치될 수 있는지 확인
;;; 결론이 OUT 상태이고 모든 선행조건이 만족되면 true
;;; 예: (check-justification my-just) => T 또는 NIL
(defun check-justification (just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

;;; 정당화의 모든 선행조건이 IN 상태인지 확인
;;; 예: (justification-satisfied? my-just) => T 또는 NIL
(defun justification-satisfied? (just)
  (every #'in-node? (just-antecedents just)))

;;; 정당화를 노드의 지지(support)로 설치하고 전파
;;; 노드를 IN 상태로 만들고, 이 변화를 의존하는 노드들에 전파
;;; 예: (install-support conclusion my-just)
;;;     => conclusion이 IN이 되고, 그에 따라 다른 노드들도 IN이 될 수 있음
(defun install-support (conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

;;; 노드가 IN이 된 것을 의존하는 다른 노드들에 전파
;;; 큐를 사용하여 영향받는 모든 노드를 IN으로 만듦
;;; 예: (propagate-inness my-node)
;;;     => my-node를 선행조건으로 사용하는 모든 정당화를 검사하여
;;;        조건이 만족되면 결론 노드들도 IN으로 만듦
(defun propagate-inness (node &aux (jtms (tms-node-jtms node)) (q (list node)))
  (do () ((null (setq node (pop q))))
    (debugging-jtms jtms "~%   Propagating belief in ~A." node)
    (dolist (justification (tms-node-consequences node))
      (when (check-justification justification)
	(make-node-in (just-consequence justification) justification)
	(push (just-consequence justification) q)))))

;;; 노드를 IN 상태로 만듦
;;; reason: 노드가 IN이 되는 이유 (정당화 또는 :ENABLED-ASSUMPTION)
;;; IN 규칙들을 큐에 넣어 실행하도록 함
;;; 예: (make-node-in my-node my-just)
;;;     => my-node의 레이블이 :IN이 되고, in-rules가 실행됨
(defun make-node-in (conseq reason &aux jtms enqueuef)
  (setq jtms (tms-node-jtms conseq)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     Making ~A in via ~A."
	     conseq
	     (if (symbolp reason)
		 reason
		 (cons (just-informant reason)
		       (mapcar (jtms-node-string jtms)
			       (just-antecedents reason)))))
  (setf (tms-node-label conseq) :IN)
  (setf (tms-node-support conseq) reason)
  (when enqueuef
    (dolist (in-rule (tms-node-in-rules conseq))
      (funcall enqueuef in-rule))
    (setf (tms-node-in-rules conseq) nil)))

;;; Assumption Manipulation

;;; 활성화된 가정을 철회(retract)
;;; 가정이 철회되면 OUT 상태가 되고, 의존하는 노드들도 OUT이 됨
;;; 그 후 대체 지지(alternative support)를 찾음
;;; 예: (retract-assumption my-assumption)
;;;     => my-assumption이 OUT이 되고, 의존 노드들도 재평가됨
(defun retract-assumption (node &aux jtms)
  (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
    (setq jtms (tms-node-jtms node))
    (debugging-jtms jtms "~%  Retracting assumption ~A." node)
    (make-node-out node)
    (find-alternative-support jtms (cons node (propagate-outness node jtms)))))

;;; 가정 노드를 활성화
;;; 가정이 아닌 노드에 대해서는 에러 발생
;;; OUT 상태의 가정을 IN으로 만들고 전파함
;;; 예: (enable-assumption my-assumption)
;;;     => my-assumption이 IN이 되고, 의존 노드들도 IN이 될 수 있음
(defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node)
    (tms-error "Can't enable the non-assumption ~A" node))
  (debugging-jtms jtms "~%  Enabling assumption ~A." node)
  (cond ((out-node? node) (make-node-in node :ENABLED-ASSUMPTION)
	                  (propagate-inness node))
	((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (null (just-antecedents (tms-node-support node)))))
	(t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
  (check-for-contradictions jtms))

;;; 노드를 OUT 상태로 만듦
;;; 지지(support)를 제거하고 레이블을 :OUT으로 설정
;;; OUT 규칙들을 큐에 넣어 실행하도록 함
;;; 예: (make-node-out my-node)
;;;     => my-node의 레이블이 :OUT이 되고, out-rules가 실행됨
(defun make-node-out (node &aux jtms enqueuef)
  (setq jtms (tms-node-jtms node)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     retracting belief in ~a." node)
  (setf (tms-node-support node) nil)
  (setf (tms-node-label node) :OUT)
  (if enqueuef (dolist (out-rule (tms-node-out-rules node))
		 (funcall enqueuef out-rule)))
  (setf (tms-node-out-rules node) nil))

;;; 노드가 OUT이 된 것을 의존하는 다른 노드들에 전파
;;; 이 노드를 사용하는 정당화들을 찾아서, 그것으로 지지받던 노드들을 OUT으로 만듦
;;; OUT이 된 노드들의 리스트를 반환하여 대체 지지를 찾을 수 있게 함
;;; 예: (propagate-outness my-node my-jtms)
;;;     => my-node에 의존하던 노드들이 OUT이 되고, 그 리스트를 반환
(defun propagate-outness (node jtms &aux out-queue)
  (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
  (do ((js (tms-node-consequences node) (append (cdr js) new))
       (new nil nil)
       (conseq nil))
      ((null js) out-queue)
    ;;For each justification using the node, check to see if
    ;;it supports some other node.  If so, forget that node,
    ;;queue up the node to look for other support, and recurse
    (setq conseq (just-consequence (car js)))
    (when (eq (tms-node-support conseq) (car js))
      (make-node-out conseq)
      (push conseq out-queue)
      (setq new (tms-node-consequences conseq)))))

;;; OUT이 된 노드들에 대해 대체 지지를 찾음
;;; 각 노드의 다른 정당화들을 검사하여 만족되는 것이 있으면 설치
;;; 예: (find-alternative-support my-jtms '(node1 node2 node3))
;;;     => node1, node2, node3에 대해 다른 정당화가 있으면 다시 IN으로 만듦
(defun find-alternative-support (jtms out-queue)
  (debugging-jtms jtms "~%   Looking for alternative supports.")
  (dolist (node out-queue)
    (unless (in-node? node)
      (dolist (just (tms-node-justs node))
	(when (check-justification just)
	  (install-support (just-consequence just)
				 just)
	  (return just))))))

;;; Contradiction handling interface

;;; 모순 검사 수행
;;; 모순 노드들 중 IN 상태인 것이 있으면 모순 처리 함수 호출
;;; 예: (check-for-contradictions my-jtms)
;;;     => 모순이 있으면 contradiction-handler 호출됨
(defun check-for-contradictions (jtms &aux contradictions)
  (when (jtms-checking-contradictions jtms)
    (dolist (cnode (jtms-contradictions jtms))
      (if (in-node? cnode) (push cnode contradictions)))
    (if contradictions
	(funcall (jtms-contradiction-handler jtms) jtms contradictions))))

;;; 모순 검사를 비활성화하고 코드 실행
;;; 예: (without-contradiction-check my-jtms
;;;       (enable-assumption node1)
;;;       (enable-assumption node2))
;;;     => 두 가정을 활성화하는 동안 모순 검사하지 않음
(defmacro without-contradiction-check (jtms &body body)
  (contradiction-check jtms nil body))

;;; 모순 검사를 활성화하고 코드 실행
;;; 예: (with-contradiction-check my-jtms
;;;       (enable-assumption node1))
;;;     => 가정 활성화 후 모순 검사 수행
(defmacro with-contradiction-check (jtms &body body)
  (contradiction-check jtms t body))

;;; 모순 검사 제어를 위한 헬퍼 함수
;;; 모순 검사 플래그를 임시로 변경하고, 완료 후 원래 값으로 복원
;;; unwind-protect를 사용하여 에러 발생 시에도 복원 보장
(defun contradiction-check (jtms flag body)
  (let ((jtmsv (gensym)) (old-value (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-value (jtms-checking-contradictions ,jtmsv)))
       (unwind-protect
	   (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
	 (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))

;;; 모순 처리 핸들러를 임시로 변경하고 코드 실행
;;; 코드 완료 후 원래 핸들러로 복원
;;; 예: (with-contradiction-handler my-jtms
;;;       #'(lambda (jtms contradictions)
;;;           (format t "Ignoring contradiction"))
;;;       (enable-assumption risky-node))
;;;     => risky-node 활성화 중 모순이 발생해도 무시
(defmacro with-contradiction-handler (jtms handler &body body)
  (let ((jtmsv (gensym)) (old-handler (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-handler (jtms-contradiction-handler ,jtmsv)))
     (unwind-protect
	 (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
       (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

;;; 기본(default) 가정들을 활성화
;;; :DEFAULT로 표시된 가정들을 활성화하되, 모순이 발생하면 철회
;;; 모순 없이 활성화할 수 있는 최대한의 기본 가정들을 활성화
;;; 예: (default-assumptions my-jtms)
;;;     => 기본 가정들을 순서대로 활성화하되, 모순 없는 것만 유지
(defun default-assumptions (jtms)
  (with-contradiction-check jtms
    (with-contradiction-handler jtms #'(lambda (&rest ignore)
					 (declare (ignore ignore))
					 (throw 'CONTRADICTION t))
      (dolist (assumption (jtms-assumptions jtms))
	(cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION))
	      ((not (eq :DEFAULT (tms-node-assumption? assumption))))
	      ((catch 'CONTRADICTION (enable-assumption assumption))
	       (retract-assumption assumption)))))))

;;; Well-founded support inqueries

;;; 노드를 지지하는 정당화 반환
;;; 예: (supporting-justification-for-node my-node) => #<Just 5>
(defun supporting-justification-for-node (node) (tms-node-support node))

;;; 노드가 의존하는 모든 가정들을 찾음
;;; 노드의 지지를 역추적하여 근거가 되는 활성화된 가정들의 리스트 반환
;;; 예: (assumptions-of-node my-node) => (#<Node: assumption1> #<Node: assumption2>)
(defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
  (do ((nodes (list node) (append (cdr nodes) new))
       (new nil nil))
      ((null nodes) assumptions)
    (let ((node (car nodes)))
      (cond ((eq (tms-node-mark node) marker))
	    ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (push node assumptions))
	    ((in-node? node)
	     (setq new (just-antecedents (tms-node-support node)))))
      (setf (tms-node-mark node) marker))))

;;; 현재 활성화된 모든 가정들의 리스트 반환
;;; 예: (enabled-assumptions my-jtms) => (#<Node: A1> #<Node: A2>)
(defun enabled-assumptions (jtms &aux result)
  (dolist (assumption (jtms-assumptions jtms) result)
    (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
	(push assumption result))))

;;; Inference engine stub to allow this JTMS to be used stand alone

;;; 노드가 왜 IN 또는 OUT 상태인지 설명 출력
;;; 활성화된 가정이면 그렇게 표시하고,
;;; 정당화로 IN이면 informant와 선행조건들을 표시
;;; 예: (why-node my-node)
;;;     출력: "node1 is IN via modus-ponens on
;;;            premise1
;;;            premise2"
(defun why-node (node &aux justification)
  (setq justification (tms-node-support node))
  (cond ((eq justification :ENABLED-ASSUMPTION)
	 (format t "~%~A is an enabled assumption"
		 (node-string node)))
	(justification
	 (format t "~%~A is IN via ~A on"
		 (node-string node)
		 (just-informant justification))
	 (dolist (anode (just-antecedents justification))
	   (format t "~%  ~A" (node-string anode))))
	(T (format t "~%~A is OUT." (node-string node))))
  node)

;;; JTMS의 모든 노드에 대해 why-node 실행
;;; 전체 네트워크의 상태를 출력
;;; 예: (why-nodes my-jtms)
;;;     => 모든 노드의 IN/OUT 상태와 이유를 출력
(defun why-nodes (jtms)
  (dolist (node (jtms-nodes jtms)) (why-node node)))

(proclaim '(special *contra-assumptions*))

;;; 기본 모순 처리 핸들러
;;; 첫 번째 모순을 처리하고 다시 모순 검사
;;; 예: (ask-user-handler my-jtms list-of-contradictions)
;;;     => 사용자에게 어떤 가정을 철회할지 물어봄
(defun ask-user-handler (jtms contradictions)
  (handle-one-contradiction (car contradictions))
  (check-for-contradictions jtms))

;;; 하나의 모순 처리
;;; 모순을 일으킨 가정들을 찾아 사용자에게 제시하고,
;;; 사용자가 선택한 가정을 철회
;;; 예: (handle-one-contradiction my-contradiction-node)
;;;     출력: "Contradiction found: (false)
;;;            1 (premise-a)
;;;            2 (premise-b)
;;;            Call (TMS-ANSWER <number>) to retract assumption."
(defun handle-one-contradiction (contra-node
				 &aux the-answer *contra-assumptions*)
  (setq *contra-assumptions* (assumptions-of-node contra-node))
  (unless *contra-assumptions*
    (tms-error "~%There is a flaw in the universe...~A" contra-node))
  (format t "~%Contradiction found: ~A" (node-string contra-node))
  (print-contra-list *contra-assumptions*)
  (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
  (setq the-answer
	(catch 'tms-contradiction-handler
	  (break "JTMS contradiction break")))
  (if (and (integerp the-answer)
	   (> the-answer 0)
	   (not (> the-answer (length *contra-assumptions*))))
      (retract-assumption (nth (1- the-answer)
			       *contra-assumptions*))))

;;; 모순을 일으킨 가정들의 번호 매긴 리스트 출력
;;; 예: (print-contra-list '(node1 node2))
;;;     출력: "1 (assumption-a)
;;;            2 (assumption-b)"
(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A" counter
	    (node-string (car nn)))))

;;; 모순 처리 시 사용자의 응답 처리
;;; 유효한 번호면 해당 가정을 철회
;;; 예: (tms-answer 2)
;;;     => *contra-assumptions*의 2번 가정이 철회됨
(defun tms-answer (num)
  (if (integerp num)
      (if (> num 0)
	  (if (not (> num (length *contra-assumptions*)))
	      (throw 'tms-contradiction-handler num)
	      (format t "~%Ignoring answer, too big."))
	  (format t "~%Ignoring answer, too small"))
      (format t "~%Ignoring answer, must be an integer.")))


;;; 정당화 네트워크를 대화형으로 탐색
;;; 노드의 지지 구조를 따라가며 어떻게 믿음이 형성되었는지 탐색
;;; 사용자가 선행조건 노드들을 선택하여 깊이 들어가고, 0으로 뒤로 가며, q로 종료
;;; 예: (explore-network my-node)
;;;     출력: "node1 is IN via rule-5 on
;;;            1. premise-a
;;;            2. premise-b
;;;            >>>
;;;     사용자 입력: 1
;;;     출력: "premise-a is an enabled assumption
;;;            >>>
;;;     사용자 입력: 0 (뒤로 가기)
;;;     사용자 입력: 2 (다른 선행조건 탐색)
;;;     사용자 입력: q (종료)
(defun explore-network (node)
  (unless (in-node? node)
	  (format t "~% Sorry, ~A not believed." (node-string node))
	  (return-from explore-network node))
  (do ((stack nil)
       (current node)
       (options nil)
       (olen 0)
       (done? nil))
      (done? current)
      (why-node current)
      (setq options (if (typep (tms-node-support current) 'just)
			(just-antecedents (tms-node-support current))))
      (setq olen (length options))
      (do ((good? nil)
	   (choice 0))
	  (good? (case good?
		       (q (return-from explore-network current))
		       (0 (if stack
			      (setq current (pop stack))
			      (return-from explore-network current)))
		       (t (push current stack)
			  (setq current (nth (1- good?) options)))))
	  (format t "~%>>>")
	  (setq choice (read))
	  (cond ((or (eq choice 'q)
		     (and (integerp choice)
			  (not (> choice olen))
			  (not (< choice 0))))
		 (setq good? choice))
		(t (format t
		    "~% Must be q or an integer from 0 to ~D."
		    olen))))))
