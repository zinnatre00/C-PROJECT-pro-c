;;; ============================================================
;;; EPS_EXPORT.lsp
;;; AutoCAD LT 2025 용 EPS 출력 자동화 리습
;;;
;;; 사용법: 명령창에 EPSOUT 입력
;;;   1. 출력 영역의 두 코너 클릭
;;;   2. 플롯 축척 입력 (예: 1:50, 1:100, 1:200)
;;;   3. 저장 경로 선택 → EPS 파일 자동 출력
;;;
;;; 출력 설정 기본값:
;;;   장치    : PostScript Level 1.pc3
;;;   용지    : ISO A3 (420.00 x 297.00 mm)
;;;   단위    : 밀리미터
;;;   방향    : 가로(L)
;;;   스타일  : DL_sol칼라.ctb
;;;   간격    : 중심
;;; ============================================================

(defun c:EPSOUT ( / *error* old-err
                   pt1 pt2
                   x1 y1 x2 y2
                   scale-input scale-str
                   filepath
                   ctb-name
                   osm )

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
  (princ "\n  EPS 출력 자동화 (AutoCAD LT 2025)")
  (princ "\n============================================")

  ;; ── 1. 출력 영역 두 코너 선택 ────────────────────────────
  (princ "\n")
  (setq pt1 (getpoint "\n[1/3] 출력 영역 왼쪽 아래 코너를 클릭하세요: "))
  (if (null pt1)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (setq pt2 (getcorner pt1 "\n[1/3] 출력 영역 오른쪽 위 코너를 클릭하세요: "))
  (if (null pt2)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; WCS 좌표 추출
  (setq x1 (rtos (car  pt1) 2 6))
  (setq y1 (rtos (cadr pt1) 2 6))
  (setq x2 (rtos (car  pt2) 2 6))
  (setq y2 (rtos (cadr pt2) 2 6))

  ;; ── 2. 플롯 축척 입력 ────────────────────────────────────
  (initget "50 100 150 200 250 300 400 500 600")
  (setq scale-input
    (getstring "\n[2/3] 플롯 축척 입력 (예: 1:50  1:100  1:200) <1:50>: "))

  (cond
    ((or (= scale-input "") (null scale-input))
     (setq scale-str "1:50"))
    ;; 숫자만 입력된 경우 (50 → 1:50)
    ((and (> (atof scale-input) 0)
          (not (vl-string-search ":" scale-input)))
     (setq scale-str (strcat "1:" scale-input)))
    (t
     (setq scale-str scale-input))
  )

  (princ (strcat "\n   축척 확인: " scale-str))

  ;; ── 3. CTB 스타일 테이블 선택 ────────────────────────────
  (setq ctb-name
    (getstring "\n   플롯 스타일 테이블 입력 <DL_sol칼라.ctb>: "))
  (if (or (null ctb-name) (= ctb-name ""))
    (setq ctb-name "DL_sol칼라.ctb")
  )

  ;; ── 4. 저장 경로 선택 ────────────────────────────────────
  (princ "\n[3/3] EPS 저장 위치를 선택하세요...")
  (setq filepath (getfiled "EPS 파일로 저장" "" "eps" 1))

  (if (null filepath)
    (progn (princ "\n저장 취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; .eps 확장자 자동 추가
  (if (not (wcmatch (strcase filepath) "*.EPS"))
    (setq filepath (strcat filepath ".eps"))
  )

  ;; ── 5. -PLOT 명령 실행 ───────────────────────────────────
  (princ "\n\nEPS 출력을 시작합니다...")
  (princ (strcat "\n  좌표  : (" x1 "," y1 ") → (" x2 "," y2 ")"))
  (princ (strcat "\n  축척  : " scale-str))
  (princ (strcat "\n  스타일: " ctb-name))
  (princ (strcat "\n  경로  : " filepath))
  (princ "\n")

  (command
    "-PLOT"
    "Y"                              ; 상세한 플롯 구성? → 예
    ""                               ; 배치 이름 → 현재 배치 유지
    "PostScript Level 1.pc3"         ; 출력 장치
    "ISO A3 (420.00 x 297.00 MM)"    ; 용지 크기
    "M"                              ; 용지 단위 → 밀리미터
    "L"                              ; 용지 방향 → 가로
    "N"                              ; 위아래 뒤집기 → 아니오
    "W"                              ; 플롯 영역 → 윈도우
    (strcat x1 "," y1)               ; 윈도우 왼쪽 아래 좌표
    (strcat x2 "," y2)               ; 윈도우 오른쪽 위 좌표
    scale-str                        ; 플롯 축척 (예: 1:50)
    "C"                              ; 플롯 간격띄우기 → 중심
    "Y"                              ; 플롯 스타일로 플롯? → 예
    ctb-name                         ; 플롯 스타일 테이블
    "N"                              ; 선가중치로 플롯? → 아니오
    "N"                              ; 선가중치를 축척에 적용? → 아니오
    "N"                              ; 도면 공간 먼저 플롯? → 아니오
    "N"                              ; 도면 공간 객체 숨기기? → 아니오
    "Y"                              ; 출력을 파일로? → 예
    filepath                         ; 저장 파일 경로
    "Y"                              ; 계속 하시겠습니까? → 예
  )

  ;; ── 6. 완료 메시지 ───────────────────────────────────────
  (setvar "OSMODE" osm)
  (princ (strcat "\n\n✔ EPS 파일이 저장되었습니다: " filepath))
  (princ "\n============================================\n")
  (princ)
)

;;; ── 단축 별칭 ────────────────────────────────────────────
(defun c:EPSEXPORT () (c:EPSOUT))

(princ "\n[EPS_EXPORT 로드 완료]  명령어: EPSOUT  또는  EPSEXPORT")
(princ)
