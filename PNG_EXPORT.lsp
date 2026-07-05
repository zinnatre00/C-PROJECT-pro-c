;;; ============================================================
;;; PNG_EXPORT.lsp
;;; AutoCAD LT 2025 용 PNG 출력 자동화 리습
;;;
;;; 사용법: 명령창에 PPNG 입력
;;;   1. 출력 영역의 두 코너 클릭 (윈도우 지정)
;;;   2. 픽셀 축척 입력 (1픽셀=N도면단위, 예: 2)
;;;   3. 저장 경로 선택 → PNG 파일 자동 출력
;;;
;;; -PLOT 명령 시퀀스 (이미지 기준):
;;;   장치    : PublishToWeb PNG.pc3
;;;   용지    : 16K(15360.00 x 8640.00 픽셀)
;;;   방향    : 가로(L)
;;;   영역    : 윈도우(W)
;;;   축척    : 출력픽셀=도면단위 (예: 2 → 1픽셀=2단위)
;;;   간격    : 중심(C)
;;;   스타일  : DL_sol칼라.ctb
;;;   선가중치: 아니오(N)
;;; ※ PNG 장치는 용지단위 선택·파일출력 여부 프롬프트가 없음
;;; ============================================================

(defun c:PPNG ( / *error*
                  pt1 pt2
                  x1 y1 x2 y2
                  px-input px-str
                  ctb-name
                  filepath
                  osm )

  ;; ── 에러 핸들러 ──────────────────────────────────────────
  (defun *error* (msg)
    (setvar "OSMODE" osm)
    (if (and msg
             (not (member msg '("함수 취소됨" "Function cancelled"
                                "quit / exit abort" ""))))
      (princ (strcat "\n오류: " msg))
    )
    (princ)
  )

  ;; ── 초기화 ───────────────────────────────────────────────
  (setq osm (getvar "OSMODE"))

  (princ "\n============================================")
  (princ "\n  PNG 출력 자동화 (AutoCAD LT 2025)")
  (princ "\n============================================")

  ;; ── 1. 출력 영역 두 코너 클릭 ────────────────────────────
  (princ "\n")
  (setq pt1 (getpoint "\n[1/3] 출력 영역 왼쪽 아래 코너를 클릭하세요: "))
  (if (null pt1)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (setq pt2 (getcorner pt1 "\n[1/3] 출력 영역 오른쪽 위 코너를 클릭하세요: "))
  (if (null pt2)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; 좌표 문자열 변환 (소수점 6자리)
  (setq x1 (rtos (car  pt1) 2 6))
  (setq y1 (rtos (cadr pt1) 2 6))
  (setq x2 (rtos (car  pt2) 2 6))
  (setq y2 (rtos (cadr pt2) 2 6))

  ;; ── 2. 픽셀 축척 입력 ────────────────────────────────────
  ;; -PLOT PNG 축척 프롬프트: "출력 픽셀=도면 단위" → 숫자 하나 입력
  ;; 이미지 예시: 2 (= 1픽셀당 2 도면단위)
  (setq px-input
    (getstring "\n[2/3] 픽셀 축척 입력 (1픽셀=N도면단위, 예: 2  4  5) <2>: "))

  (cond
    ((or (null px-input) (= px-input ""))
     (setq px-str "2"))
    ((> (atof px-input) 0)
     (setq px-str px-input))
    (t
     (setq px-str "2"))
  )

  (princ (strcat "\n   축척 확인: 1픽셀 = " px-str " 도면단위"))

  ;; ── 3. CTB 스타일 테이블 입력 ────────────────────────────
  (setq ctb-name
    (getstring "\n   플롯 스타일 테이블 입력 <DL_sol칼라.ctb>: "))
  (if (or (null ctb-name) (= ctb-name ""))
    (setq ctb-name "DL_sol칼라.ctb")
  )

  ;; ── 4. 저장 경로 선택 ────────────────────────────────────
  (princ "\n[3/3] PNG 저장 위치를 선택하세요...")
  (setq filepath (getfiled "PNG 파일로 저장" "" "png" 1))

  (if (null filepath)
    (progn (princ "\n저장 취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  ;; .png 확장자 자동 추가
  (if (not (wcmatch (strcase filepath) "*.PNG"))
    (setq filepath (strcat filepath ".png"))
  )

  ;; ── 5. -PLOT 명령 실행 ───────────────────────────────────
  (princ "\n\nPNG 출력을 시작합니다...")
  (princ (strcat "\n  좌표  : (" x1 ", " y1 ") → (" x2 ", " y2 ")"))
  (princ (strcat "\n  축척  : 1픽셀 = " px-str " 단위"))
  (princ (strcat "\n  스타일: " ctb-name))
  (princ (strcat "\n  경로  : " filepath))
  (princ "\n")

  ;; ※ PNG 장치 -PLOT 시퀀스 (이미지 순서 그대로):
  ;;   Y  → 상세한 플롯 구성
  ;;   "" → 배치 이름 (현재 배치)
  ;;   장치명 / 용지명 / 방향 / 뒤집기 / 영역
  ;;   좌표1 / 좌표2
  ;;   축척 (숫자만) / 간격 / 스타일여부 / CTB명
  ;;   선가중치 / 선가중치축척 / 공간순서 / 객체숨기기
  ;;   파일경로  ← PNG는 항상 파일 출력이므로 바로 경로 입력
  (command
    "-PLOT"
    "Y"                              ; 상세한 플롯 구성? → 예
    ""                               ; 배치 이름 → 현재 배치 유지 (엔터)
    "PublishToWeb PNG.pc3"           ; 출력 장치 이름
    "16K(15360.00 x 8640.00 픽셀)"  ; 용지 크기
    "L"                              ; 용지 방향 → 가로
    "N"                              ; 위아래 뒤집기 → 아니오
    "W"                              ; 플롯 영역 → 윈도우
    (strcat x1 "," y1)              ; 윈도우 왼쪽 아래 좌표
    (strcat x2 "," y2)              ; 윈도우 오른쪽 위 좌표
    px-str                           ; 축척 (출력픽셀=도면단위, 예: 2)
    "C"                              ; 간격띄우기 → 중심
    "Y"                              ; 플롯 스타일로 플롯? → 예
    ctb-name                         ; 플롯 스타일 테이블
    "N"                              ; 선가중치로 플롯? → 아니오
    "N"                              ; 선가중치를 축척에 적용? → 아니오
    "N"                              ; 도면 공간 먼저 플롯? → 아니오
    "N"                              ; 도면 공간 객체 숨기기? → 아니오
    filepath                         ; PNG 저장 경로 (파일 출력 여부 없이 바로)
    "Y"                              ; 계속 하시겠습니까? → 예
  )

  ;; ── 6. 완료 메시지 ───────────────────────────────────────
  (setvar "OSMODE" osm)
  (princ (strcat "\n\n[완료] PNG 파일이 저장되었습니다: " filepath))
  (princ "\n============================================\n")
  (princ)
)

;;; ── 단축 별칭 ────────────────────────────────────────────
(defun c:PNGOUT () (c:PPNG))

(princ "\n[PNG_EXPORT 로드 완료]  명령어: PPNG  또는  PNGOUT")
(princ)
