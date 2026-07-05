;; PNG_EXPORT.lsp - AutoCAD LT 2025 PNG 출력 자동화
;; 명령: PPNG
;; 1. 출력 영역 두 코너 클릭
;; 2. 픽셀 축척 입력 (1픽셀=N단위, 예: 2)
;; 3. 저장 경로 선택 -> PNG 파일 출력
;; 장치: PublishToWeb PNG.pc3 / 16K(15360x8640 픽셀) / 가로 / DL_sol칼라.ctb

(defun c:PPNG ( / *error*
                  pt1 pt2
                  x1 y1 x2 y2
                  px-input px-str
                  ctb-name
                  filepath
                  osm )

  (defun *error* (msg)
    (setvar "OSMODE" osm)
    (if (and msg (not (member msg (list "함수 취소됨" "Function cancelled"
                                       "quit / exit abort" ""))))
      (princ (strcat "\n오류: " msg))
    )
    (princ)
  )

  (setq osm (getvar "OSMODE"))

  (princ "\n============================================")
  (princ "\n PNG 출력 자동화 (AutoCAD LT 2025)")
  (princ "\n============================================")

  ;; 1. 출력 영역 두 코너 클릭
  (princ "\n")
  (setq pt1 (getpoint "\n[1/3] 출력 영역 왼쪽 아래 코너를 클릭하세요: "))
  (if (null pt1)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (setq pt2 (getcorner pt1 "\n[1/3] 출력 영역 오른쪽 위 코너를 클릭하세요: "))
  (if (null pt2)
    (progn (princ "\n취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (setq x1 (rtos (car  pt1) 2 6))
  (setq y1 (rtos (cadr pt1) 2 6))
  (setq x2 (rtos (car  pt2) 2 6))
  (setq y2 (rtos (cadr pt2) 2 6))

  ;; 2. 픽셀 축척 입력 (출력픽셀=도면단위, 이미지 예시: 2)
  (setq px-input
    (getstring "\n[2/3] 픽셀 축척 입력 (예: 2=1픽셀당2단위, 4, 5) <2>: "))

  (cond
    ((or (null px-input) (= px-input "")) (setq px-str "2"))
    ((> (atof px-input) 0)               (setq px-str px-input))
    (t                                   (setq px-str "2"))
  )

  (princ (strcat "\n   축척 확인: 1픽셀 = " px-str " 도면단위"))

  ;; 3. CTB 스타일 테이블
  (setq ctb-name
    (getstring "\n   플롯 스타일 테이블 입력 <DL_sol칼라.ctb>: "))
  (if (or (null ctb-name) (= ctb-name ""))
    (setq ctb-name "DL_sol칼라.ctb")
  )

  ;; 4. 저장 경로 선택
  (princ "\n[3/3] PNG 저장 위치를 선택하세요...")
  (setq filepath (getfiled "PNG 파일로 저장" "" "png" 1))

  (if (null filepath)
    (progn (princ "\n저장 취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (if (not (wcmatch (strcase filepath) "*.PNG"))
    (setq filepath (strcat filepath ".png"))
  )

  ;; 5. -PLOT 명령 실행
  (princ "\n\nPNG 출력을 시작합니다...")
  (princ (strcat "\n  좌표  : (" x1 ", " y1 ") / (" x2 ", " y2 ")"))
  (princ (strcat "\n  축척  : 1픽셀 = " px-str " 단위"))
  (princ (strcat "\n  스타일: " ctb-name))
  (princ (strcat "\n  경로  : " filepath))
  (princ "\n")

  ;; -PLOT 시퀀스 (이미지 순서):
  ;; Y / 배치명 / 장치 / 용지 / 방향 / 뒤집기 / 영역
  ;; 좌표1 / 좌표2 / 축척(숫자) / 간격 / 스타일여부 / CTB
  ;; 선가중치 / 선가중치축척 / 공간순서 / 객체숨기기 / 파일경로 / 확인
  ;; (PNG 장치: 용지단위 프롬프트 없음, 파일출력 여부 프롬프트 없음)
  (command
    "-PLOT"
    "Y"
    ""
    "PublishToWeb PNG.pc3"
    "16K(15360.00 x 8640.00 픽셀)"
    "L"
    "N"
    "W"
    (strcat x1 "," y1)
    (strcat x2 "," y2)
    px-str
    "C"
    "Y"
    ctb-name
    "N"
    "N"
    "N"
    "N"
    filepath
    "Y"
  )

  ;; 6. 완료
  (setvar "OSMODE" osm)
  (princ (strcat "\n\n[완료] PNG 저장 완료: " filepath))
  (princ "\n============================================\n")
  (princ)
)

(defun c:PNGOUT () (c:PPNG))

(princ "\n[PNG_EXPORT 로드 완료]  명령어: PPNG  또는  PNGOUT")
(princ)
