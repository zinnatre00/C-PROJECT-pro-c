;;; ============================================================
;;; ELEVATION_DRAW.lsp
;;; AutoCAD LT 2025 용 입면 작성 리습
;;;
;;; 사용법: 명령창에 ELV 입력
;;;   1. 평면 사각형의 시작 코너점 클릭
;;;   2. 시계방향으로 다음 코너점 클릭 (이 두 점 사이 거리 = 입면 폭)
;;;   3. 높이 입력 (mm 또는 도면 단위)
;;;   4. 입면 삽입 기준점 클릭
;;;   → 입면 사각형 자동 작성 (폭 × 높이)
;;;
;;; 작성 내용:
;;;   - 외곽 사각형 (4선 또는 RECTANG)
;;;   - 선택 두 점의 방향각을 반영한 회전 옵션 포함
;;; ============================================================

(defun C:ELV ( / *error* osm
                 pt1 pt2
                 width height ang
                 ins_pt
                 p0 p1 p2 p3
                 rotate-flag rot-ang
                 ename )

  ;; ── 에러 핸들러 ──────────────────────────────────────────
  (defun *error* (msg)
    (setvar "OSMODE" osm)
    (if (and msg (not (member msg '("함수 취소됨" "quit / exit abort"
                                   "Function cancelled" ""))))
      (princ (strcat "\n오류: " msg))
    )
    (princ)
  )

  ;; ── 초기화 ───────────────────────────────────────────────
  (setq osm (getvar "OSMODE"))

  (princ "\n============================================")
  (princ "\n  입면 작성 루틴 (AutoCAD LT 2025)")
  (princ "\n  평면 두 점 선택 → 높이 입력 → 입면 사각형")
  (princ "\n============================================")

  ;; ── 1. 평면 시작점 선택 ──────────────────────────────────
  (setq pt1 (getpoint "\n[1/4] 평면 시작 코너점을 클릭하세요: "))
  (if (null pt1)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; ── 2. 시계방향 다음점 선택 (입면 폭 결정) ───────────────
  (setq pt2 (getpoint pt1 "\n[2/4] 시계방향 다음 코너점을 클릭하세요 (이 거리 = 입면 폭): "))
  (if (null pt2)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; 두 점 거리 = 입면 폭
  (setq width (distance pt1 pt2))
  (setq ang   (angle pt1 pt2))   ; 두 점의 방향각 (라디안)

  (if (< width 1e-6)
    (progn
      (princ "\n두 점이 너무 가깝습니다. 다시 시도하세요.")
      (setvar "OSMODE" osm)
      (princ)
      (exit)
    )
  )

  (princ (strcat "\n   → 입면 폭: " (rtos width 2 2) " (단위)"))

  ;; ── 3. 높이 입력 ─────────────────────────────────────────
  (initget 6)   ; 음수·영값 불허
  (setq height (getdist "\n[3/4] 입면 높이를 입력하세요: "))
  (if (null height)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (princ (strcat "\n   → 입면 높이: " (rtos height 2 2) " (단위)"))

  ;; ── 4. 회전 여부 선택 ────────────────────────────────────
  ;; 두 점 방향각이 0(정동) 이 아니면 회전 여부 물음
  (setq rotate-flag nil)
  (setq rot-ang 0.0)

  (if (> (abs ang) 1e-6)
    (progn
      (initget "예 아니오 Y N")
      (setq rotate-flag
        (getkword
          (strcat "\n   선택 방향각: "
                  (rtos (* ang (/ 180.0 pi)) 2 2)
                  "°  → 이 방향으로 입면을 회전할까요? [예(Y)/아니오(N)] <아니오>: ")))
      (if (member rotate-flag '("예" "Y"))
        (setq rot-ang ang)
        (setq rot-ang 0.0)
      )
    )
  )

  ;; ── 5. 입면 삽입 기준점 선택 ─────────────────────────────
  (setq ins_pt (getpoint "\n[4/4] 입면 사각형의 왼쪽 아래 기준점을 클릭하세요: "))
  (if (null ins_pt)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; ── 6. 사각형 꼭짓점 계산 (회전 적용) ───────────────────
  ;;  p0 = 기준점 (좌하)
  ;;  p1 = 우하  (기준점 + 폭 방향)
  ;;  p2 = 우상  (p1 + 높이 방향, 높이는 항상 Z-UP 수직)
  ;;  p3 = 좌상  (p0 + 높이 방향)
  ;;
  ;;  회전 없음 → 입면은 항상 수평 폭 × 수직 높이
  ;;  회전 있음 → 폭 방향만 rot-ang 적용, 높이는 그에 수직

  (setq p0 (list (car ins_pt) (cadr ins_pt) 0.0))

  (if (> (abs rot-ang) 1e-6)
    ;; 회전 있는 경우: 폭 방향 = rot-ang, 높이 방향 = rot-ang + 90°
    (progn
      (setq p1 (list (+ (car p0) (* width (cos rot-ang)))
                     (+ (cadr p0) (* width (sin rot-ang)))
                     0.0))
      (setq p3 (list (+ (car p0) (* height (cos (+ rot-ang (* 0.5 pi)))))
                     (+ (cadr p0) (* height (sin (+ rot-ang (* 0.5 pi)))))
                     0.0))
      (setq p2 (list (+ (car p1) (* height (cos (+ rot-ang (* 0.5 pi)))))
                     (+ (cadr p1) (* height (sin (+ rot-ang (* 0.5 pi)))))
                     0.0))
    )
    ;; 회전 없는 경우: 폭 = X축, 높이 = Y축
    (progn
      (setq p1 (list (+ (car p0) width) (cadr p0)           0.0))
      (setq p2 (list (+ (car p0) width) (+ (cadr p0) height) 0.0))
      (setq p3 (list (car p0)           (+ (cadr p0) height) 0.0))
    )
  )

  ;; ── 7. 입면 사각형 선 그리기 (4개의 LINE) ────────────────
  (princ "\n입면 사각형을 작성합니다...")

  (command "LINE" p0 p1 p2 p3 "닫기")

  ;; ── 8. 완료 메시지 ───────────────────────────────────────
  (setvar "OSMODE" osm)
  (princ "\n")
  (princ (strcat "\n✔ 입면 작성 완료"))
  (princ (strcat "\n   폭   : " (rtos width  2 2)))
  (princ (strcat "\n   높이 : " (rtos height 2 2)))
  (if (> (abs rot-ang) 1e-6)
    (princ (strcat "\n   회전 : " (rtos (* rot-ang (/ 180.0 pi)) 2 2) "°"))
  )
  (princ "\n============================================\n")
  (princ)
)

;;; ── 단축 별칭 ────────────────────────────────────────────
(defun C:ELEVATION () (C:ELV))
(defun C:입면     () (C:ELV))

(princ "\n[ELEVATION_DRAW 로드 완료]")
(princ "\n명령어: ELV  또는  ELEVATION  또는  입면")
(princ)
