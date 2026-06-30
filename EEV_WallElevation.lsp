;;; ============================================================
;;; EEV_WallElevation.lsp
;;; 명령어: EEV
;;; 기능: 선택한 벽체 선(Line/Polyline)의 같은 각도상의 선분
;;;       길이를 추출하여 입면도(직사각형)를 자동으로 생성
;;; ============================================================

(defun c:eev ( / *error*
                 osmode-saved
                 ss-walls ang-ref
                 wall-length wall-height
                 ins-pt
                 curtain-opt
                 layer-name)

  ;; ── 내부 에러 핸들러 ──────────────────────────────────────
  (defun *error* (msg)
    (if osmode-saved (setvar "OSMODE" osmode-saved))
    (setvar "CMDECHO" 1)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\n오류: " msg))
    )
    (princ)
  )

  ;; ── 초기 설정 저장 ────────────────────────────────────────
  (setq osmode-saved (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ;; ── E-WALL 레이어 확인/생성 ──────────────────────────────
  (setq layer-name "E-WALL")
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (command "_.LAYER" "_N" layer-name "_C" "3" layer-name "")
    )
  )

  ;; ── 1단계: 벽체 선 선택 ──────────────────────────────────
  (princ "\n[EEV] 입면을 추출할 벽체 선(Line/Polyline)을 선택하세요.")
  (setq ss-walls
    (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE")))
  )
  (if (null ss-walls)
    (progn
      (princ "\n선택이 취소되었습니다.")
      (setvar "OSMODE" osmode-saved)
      (setvar "CMDECHO" 1)
      (exit)
    )
  )

  ;; ── 2단계: 기준 각도 추출 (첫 번째 선분 기준) ───────────
  (setq ang-ref (eev-get-ref-angle (ssname ss-walls 0)))
  (if (null ang-ref)
    (progn
      (princ "\n각도를 추출할 수 없습니다.")
      (setvar "OSMODE" osmode-saved)
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  (princ (strcat "\n기준 각도: " (rtos (angtos ang-ref 4 4) 2 4) "도"))

  ;; ── 3단계: 같은 각도 선분의 총 길이 합산 ─────────────────
  (setq wall-length (eev-sum-length-by-angle ss-walls ang-ref))
  (princ (strcat "\n추출된 벽체 총 길이: " (rtos wall-length 2 2) "mm"))

  ;; ── 4단계: 높이 입력 ─────────────────────────────────────
  (initget 6) ; 0 이하 거부
  (setq wall-height
    (getdist (strcat "\n벽체 높이를 입력하세요 <2400>: "))
  )
  (if (null wall-height) (setq wall-height 2400.0))

  ;; ── 5단계: 커튼박스 옵션 선택 ────────────────────────────
  (princ "\n커튼박스 추가 위치를 선택하세요.")
  (princ "\n  [1] 좌코너 위측  [2] 우코너 위측  [3] 없음")
  (initget 1 "1 2 3")
  (setq curtain-opt (getkword "\n선택 <3>: "))
  (if (null curtain-opt) (setq curtain-opt "3"))

  ;; ── 6단계: 기준점 지정 ───────────────────────────────────
  (setvar "OSMODE" osmode-saved) ; 기준점 지정 시 OSNAP 복구
  (setq ins-pt (getpoint "\n입면을 그릴 기준 위치(좌하단)를 클릭하세요: "))
  (if (null ins-pt)
    (progn
      (princ "\n위치 지정이 취소되었습니다.")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  (setvar "OSMODE" 0) ; 그리기 시 OSNAP 끄기

  ;; ── 7단계: 레이어 설정 후 입면 그리기 ───────────────────
  (command "_.LAYER" "_S" layer-name "")
  (eev-draw-elevation ins-pt wall-length wall-height curtain-opt)

  ;; ── 복구 및 종료 ─────────────────────────────────────────
  (setvar "OSMODE" osmode-saved)
  (setvar "CMDECHO" 1)
  (princ (strcat "\n[EEV] 완료. 레이어 [" layer-name "] 에 입면이 생성되었습니다."))
  (princ)
)


;;; ── 첫 번째 객체에서 기준 각도 추출 ─────────────────────────
(defun eev-get-ref-angle (ent / ed etype p1 p2 bulge i vcount)
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (cond
    ;; LINE
    ((= etype "LINE")
     (setq p1 (cdr (assoc 10 ed))
           p2 (cdr (assoc 11 ed)))
     (angle p1 p2)
    )
    ;; LWPOLYLINE: 첫 번째 세그먼트
    ((= etype "LWPOLYLINE")
     (setq vcount (cdr (assoc 90 ed)))
     (if (>= vcount 2)
       (progn
         (setq p1 (eev-lwpoly-vertex ed 0)
               p2 (eev-lwpoly-vertex ed 1))
         (if (and p1 p2)
           (angle p1 p2)
           nil
         )
       )
       nil
     )
    )
    (T nil)
  )
)


;;; ── LWPOLYLINE에서 n번째 꼭짓점 좌표 추출 ────────────────────
(defun eev-lwpoly-vertex (ed idx / pts cnt)
  (setq pts '() cnt 0)
  (foreach pair ed
    (if (= (car pair) 10)
      (setq pts (append pts (list (cdr pair))))
    )
  )
  (if (< idx (length pts))
    (nth idx pts)
    nil
  )
)


;;; ── 선택 세트에서 기준 각도와 같은 방향 선분 길이 합산 ──────
;;; 각도 비교 허용 오차: 1도 (0.01745 rad)
(defun eev-sum-length-by-angle (ss ang-ref / i ent ed etype total ang-seg len)
  (setq total 0.0)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq ed (entget ent))
    (setq etype (cdr (assoc 0 ed)))
    (cond
      ((= etype "LINE")
       (setq ang-seg (eev-line-angle ed))
       (if (eev-angle-match ang-seg ang-ref)
         (setq total (+ total (eev-line-length ed)))
       )
      )
      ((= etype "LWPOLYLINE")
       (setq total (+ total (eev-lwpoly-length-by-angle ed ang-ref)))
      )
    )
    (setq i (1+ i))
  )
  total
)


;;; ── LINE 각도 추출 ───────────────────────────────────────────
(defun eev-line-angle (ed / p1 p2)
  (setq p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (angle p1 p2)
)

;;; ── LINE 길이 ────────────────────────────────────────────────
(defun eev-line-length (ed / p1 p2)
  (setq p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (distance p1 p2)
)


;;; ── LWPOLYLINE 에서 기준 각도와 일치하는 세그먼트 길이 합산 ──
(defun eev-lwpoly-length-by-angle (ed ang-ref / pts total k p1 p2 seg-ang)
  (setq pts '() total 0.0)
  (foreach pair ed
    (if (= (car pair) 10)
      (setq pts (append pts (list (cdr pair))))
    )
  )
  (setq k 0)
  (while (< k (1- (length pts)))
    (setq p1 (nth k pts)
          p2 (nth (1+ k) pts))
    (setq seg-ang (angle p1 p2))
    (if (eev-angle-match seg-ang ang-ref)
      (setq total (+ total (distance p1 p2)))
    )
    (setq k (1+ k))
  )
  total
)


;;; ── 각도 일치 여부 확인 (정방향 + 역방향, 허용오차 1도) ──────
(defun eev-angle-match (a1 a2 / diff pi2)
  (setq pi2 (* pi 2.0))
  ;; 정규화 0 ~ 2pi
  (setq a1 (rem (+ (rem a1 pi2) pi2) pi2))
  (setq a2 (rem (+ (rem a2 pi2) pi2) pi2))
  (setq diff (abs (- a1 a2)))
  (if (> diff pi) (setq diff (- pi2 diff)))
  ;; 정방향 또는 역방향(pi 차이) 모두 허용
  (or (< diff 0.01745)
      (< (abs (- diff pi)) 0.01745))
)


;;; ── 입면 직사각형 + 커튼박스 그리기 ─────────────────────────
(defun eev-draw-elevation (pt0 width height curtain-opt
                           / x0 y0 x1 y1
                             cb-w cb-h)
  (setq x0 (car pt0)
        y0 (cadr pt0)
        x1 (+ x0 width)
        y1 (+ y0 height))

  ;; 본체 사각형
  (command "_.PLINE"
    (list x0 y0)
    (list x1 y0)
    (list x1 y1)
    (list x0 y1)
    "_C"
  )

  ;; 커튼박스 (가로200 × 세로130)
  (setq cb-w 200.0  cb-h 130.0)
  (cond
    ;; 좌코너 위측
    ((= curtain-opt "1")
     (command "_.PLINE"
       (list x0 y1)
       (list (+ x0 cb-w) y1)
       (list (+ x0 cb-w) (+ y1 cb-h))
       (list x0 (+ y1 cb-h))
       "_C"
     )
    )
    ;; 우코너 위측
    ((= curtain-opt "2")
     (command "_.PLINE"
       (list (- x1 cb-w) y1)
       (list x1 y1)
       (list x1 (+ y1 cb-h))
       (list (- x1 cb-w) (+ y1 cb-h))
       "_C"
     )
    )
    ;; 없음 → 아무것도 안 그림
  )
)

(princ "\n[EEV] 벽체 입면 생성 명령어 로드 완료. 'EEV' 를 입력하여 실행하세요.")
(princ)
