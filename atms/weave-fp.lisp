;; -*- Mode: Lisp; -*-

;;;; TCO Functional Programming version of weave
;; Converted from atms.lisp (Forbus & de Kleer, 1993)
;;
;; 원본의 명령형(imperative) 코드를 Tail Call Optimization이 가능한
;; 재귀적 함수형 프로그래밍 스타일로 변환합니다.
;;
;; 설계 원칙:
;;   1. 모든 루프 → tail-recursive 내부 함수 (labels 사용)
;;   2. 상태 변이(mutation) 없음: rplaca/setq 대신 accumulator 패턴
;;   3. 조기 종료(early termination) → nil 반환으로 표현
;;   4. :DOMINATED sentinel으로 "삽입 불요" 신호
;;
;; 변환 전 원본 weave (atms.lisp:245-263):
;;
;;   (defun weave (antecedent envs antecedents &aux new-envs new-env)
;;     (setq envs (copy-list envs))
;;     (dolist (node antecedents)
;;       (unless (eq node antecedent)
;;         (setq new-envs nil)
;;         (dolist (env envs)
;;           (if env
;;               (dolist (node-env (tms-node-label node))
;;                 (setq new-env (union-env env node-env))
;;                 (unless (env-nogood? new-env)
;;                   (do ((nnew-envs new-envs (cdr nnew-envs)))
;;                       ((null nnew-envs) (push new-env new-envs))
;;                     (when (car nnew-envs)
;;                       (case (compare-env new-env (car nnew-envs))
;;                         ((:EQ :S21) (return nil))
;;                         (:S12 (rplaca nnew-envs nil)))))))))
;;         (setq envs (delete nil new-envs :TEST #'eq))
;;         (unless envs (return-from weave nil))))
;;     envs)

(in-package :COMMON-LISP-USER)

;;; ─────────────────────────────────────────────────────────────────
;;; Level 3: insert-minimal
;;; ─────────────────────────────────────────────────────────────────
;;; new-env를 acc(antichain 리스트)에 삽입하되, antichain 불변식을 유지합니다.
;;;
;;; 원본의 do 루프:
;;;   (do ((nnew-envs new-envs ...))
;;;       ((null nnew-envs) (push new-env new-envs))
;;;     (case (compare-env ...) (:EQ/:S21 return) (:S12 rplaca)))
;;;
;;; TCO 변환:
;;;   - remaining: 아직 비교하지 않은 acc 원소들
;;;   - kept:      비교 후 유지할 원소들 (역순 accumulator)
;;;   - 반환값: :DOMINATED (삽입 불요) | 새 antichain 리스트
;;;
;;; 모든 재귀 호출이 꼬리 위치(tail position)에 있어 TCO 가능.

(defun insert-minimal-fp (new-env remaining kept)
  (cond
    ;; 스캔 완료: new-env가 지배당하지 않음 → 앞에 추가
    ((null remaining)
     (cons new-env (nreverse kept)))

    (t
     (let ((item (car remaining)))
       (case (compare-env new-env item)
         ;; ① new-env ⊇ item (or equal) → new-env가 지배당함 → 삽입 불요
         ((:EQ :S21) :DOMINATED)

         ;; ② new-env ⊂ item → item이 지배당함 → item 제거 후 계속
         (:S12
          (insert-minimal-fp new-env (cdr remaining) kept))

         ;; ③ 관계 없음 → item 유지 후 계속
         (t
          (insert-minimal-fp new-env (cdr remaining)
                             (cons item kept))))))))


;;; ─────────────────────────────────────────────────────────────────
;;; Level 2a: process-node-envs
;;; ─────────────────────────────────────────────────────────────────
;;; 하나의 env에 대해 node의 label 내 모든 node-env와 교차곱(cross product)을
;;; 계산하여 acc에 누적합니다.
;;;
;;; 원본의 내부 dolist:
;;;   (dolist (node-env (tms-node-label node))
;;;     (setq new-env (union-env env node-env))
;;;     (unless (env-nogood? new-env) ...insert...))
;;;
;;; TCO 변환: remaining-node-label을 소진하며 tail-recursion

(defun process-node-envs-fp (env remaining-node-label acc)
  (cond
    ;; node-label 소진 → 현재 acc 반환
    ((null remaining-node-label) acc)

    (t
     (let* ((node-env (car remaining-node-label))
            (merged   (union-env env node-env)))
       (if (or (null merged) (env-nogood? merged))
           ;; nogood → 이 조합은 건너뜀
           (process-node-envs-fp env (cdr remaining-node-label) acc)
           ;; valid → antichain 삽입 시도
           (let ((result (insert-minimal-fp merged acc '())))
             (process-node-envs-fp
              env
              (cdr remaining-node-label)
              (if (eq result :DOMINATED) acc result))))))))


;;; ─────────────────────────────────────────────────────────────────
;;; Level 2b: process-envs
;;; ─────────────────────────────────────────────────────────────────
;;; remaining-envs의 각 env에 대해 node-label과 교차곱을 수행하고 acc에 누적합니다.
;;;
;;; 원본의 외부 dolist:
;;;   (dolist (env envs) ...)
;;;   → envs 리스트를 소진하는 tail-recursion

(defun process-envs-fp (remaining-envs node-label acc)
  (cond
    ;; 모든 env 처리 완료
    ((null remaining-envs) acc)

    (t
     (let ((new-acc (process-node-envs-fp
                     (car remaining-envs) node-label acc)))
       ;; new-acc를 가지고 다음 env로 tail-recursion
       (process-envs-fp (cdr remaining-envs) node-label new-acc)))))


;;; ─────────────────────────────────────────────────────────────────
;;; Level 1: fold-antecedents
;;; ─────────────────────────────────────────────────────────────────
;;; antecedents 리스트를 순회하며 envs를 점진적으로 좁혀갑니다.
;;; antecedent 자신은 건너뜁니다.
;;;
;;; 원본의 최외곽 dolist:
;;;   (dolist (node antecedents) (unless (eq node antecedent) ...))
;;; 조기 종료:
;;;   (unless envs (return-from weave nil))

(defun fold-antecedents-fp (antecedent remaining current-envs)
  (cond
    ;; antecedents 소진 → 최종 envs 반환
    ((null remaining) current-envs)

    (t
     (let ((node (car remaining)))
       (if (eq node antecedent)
           ;; 자기 자신 건너뜀 (증분 계산의 핵심)
           (fold-antecedents-fp antecedent (cdr remaining) current-envs)
           ;; 이 node의 label과 교차곱 수행
           (let ((new-envs (process-envs-fp
                            current-envs
                            (tms-node-label node)
                            nil)))
             (if (null new-envs)
                 ;; 조기 종료: 가능한 world가 없음
                 nil
                 ;; 다음 antecedent로 tail-recursion
                 (fold-antecedents-fp antecedent (cdr remaining) new-envs))))))))


;;; ─────────────────────────────────────────────────────────────────
;;; weave-fp: 진입점
;;; ─────────────────────────────────────────────────────────────────
;;; 원본 weave와 동일한 시그니처, 동일한 의미론(semantics).
;;; 내부 구현만 순수 함수형 + TCO로 변환됨.

(defun weave-fp (antecedent envs antecedents)
  "TCO Functional Programming version of weave.

   antecedent: 이 justification을 현재 propagate 중인 노드 (건너뜀)
   envs: antecedent 노드의 현재 label (시작 env 집합)
   antecedents: justification의 전체 antecedent 목록

   반환: 새로운 minimal env 리스트, 또는 nil (불가능한 경우)"
  (fold-antecedents-fp antecedent antecedents envs))


;;; ─────────────────────────────────────────────────────────────────
;;; 원본 weave와 비교 테스트
;;; ─────────────────────────────────────────────────────────────────
;;; 아래 테스트는 동일 ATMS 컨텍스트에서 두 함수의 결과를 비교합니다.

(defun test-weave-equivalence (&optional (*atre* *atre*))
  "원본 weave와 weave-fp의 결과를 비교합니다."
  (format t "~%=== weave vs weave-fp 동등성 검사 ===~%")
  (let* ((atms (atre-atms *atre*))
         (a (tms-create-node atms 'A :ASSUMPTIONP t))
         (b (tms-create-node atms 'B :ASSUMPTIONP t))
         (c (tms-create-node atms 'C :ASSUMPTIONP t))
         (n (tms-create-node atms 'N))
         (just (make-just :INFORMANT 'test
                          :CONSEQUENCE n
                          :ANTECEDENTS (list a b c)))
         (start-envs (tms-node-label a)))
    (let ((r1 (weave a start-envs (list a b c)))
          (r2 (weave-fp a start-envs (list a b c))))
      (format t "~%원본 weave 결과:    ~A" r1)
      (format t "~%weave-fp 결과:     ~A" r2)
      (format t "~%결과 동등:          ~A~%" (equal r1 r2)))))
