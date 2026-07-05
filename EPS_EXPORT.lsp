;; EPS_EXPORT.lsp - AutoCAD LT 2025 EPS 출력 자동화
;; 명령: EPSOUT
;; 1. 출력 영역 두 코너 클릭
;; 2. 플롯 축척 입력 (예: 1:50, 1:100, 1:200)
;; 3. 저장 경로 선택 -> EPS 파일 출력
;; 장치: PostScript Level 1.pc3 / ISO A3 420x297mm / 가로 / DL_sol칼라.ctb

(defun c:EPSOUT ( / *error*
                    pt1 pt2
                    x1 y1 x2 y2
                    scale-input scale-str
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
  (princ "\n EPS 출력 자동화 (AutoCAD LT 2025)")
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

  ;; 2. 플롯 축척 입력 (예: 1:50, 숫자만 입력시 자동으로 1:N 변환)
  (setq scale-input
    (getstring "\n[2/3] 플롯 축척 입력 (예: 1:50  1:100  1:200) <1:50>: "))

  (cond
    ((or (null scale-input) (= scale-input ""))
     (setq scale-str "1:50"))
    ((and (> (atof scale-input) 0)
          (not (vl-string-search ":" scale-input)))
     (setq scale-str (strcat "1:" scale-input)))
    (t
     (setq scale-str scale-input))
  )

  (princ (strcat "\n   축척 확인: " scale-str))

  ;; 3. CTB 스타일 테이블
  (setq ctb-name
    (getstring "\n   플롯 스타일 테이블 입력 <DL_sol칼라.ctb>: "))
  (if (or (null ctb-name) (= ctb-name ""))
    (setq ctb-name "DL_sol칼라.ctb")
  )

  ;; 4. 저장 경로 선택
  (princ "\n[3/3] EPS 저장 위치를 선택하세요...")
  (setq filepath (getfiled "EPS 파일로 저장" "" "eps" 1))

  (if (null filepath)
    (progn (princ "\n저장 취소되었습니다.") (setvar "OSMODE" osm) (princ) (exit))
  )

  (if (not (wcmatch (strcase filepath) "*.EPS"))
    (setq filepath (strcat filepath ".eps"))
  )

  ;; 5. -PLOT 명령 실행
  (princ "\n\nEPS 출력을 시작합니다...")
  (princ (strcat "\n  좌표  : (" x1 ", " y1 ") / (" x2 ", " y2 ")"))
  (princ (strcat "\n  축척  : " scale-str))
  (princ (strcat "\n  스타일: " ctb-name))
  (princ (strcat "\n  경로  : " filepath))
  (princ "\n")

  ;; -PLOT 시퀀스:
  ;; Y / 배치명 / 장치 / 용지 / 단위(M) / 방향 / 뒤집기 / 영역
  ;; 좌표1 / 좌표2 / 축척 / 간격 / 스타일여부 / CTB
  ;; 선가중치 / 선가중치축척 / 공간순서 / 객체숨기기 / 파일출력Y / 경로 / 확인
  (command
    "-PLOT"
    "Y"
    ""
    "PostScript Level 1.pc3"
    "ISO A3 (420.00 x 297.00 MM)"
    "M"
    "L"
    "N"
    "W"
    (strcat x1 "," y1)
    (strcat x2 "," y2)
    scale-str
    "C"
    "Y"
    ctb-name
    "N"
    "N"
    "N"
    "N"
    "Y"
    filepath
    "Y"
  )

  ;; 6. 완료
  (setvar "OSMODE" osm)
  (princ (strcat "\n\n[완료] EPS 저장 완료: " filepath))
  (princ "\n============================================\n")
  (princ)
)

(defun c:EPSEXPORT () (c:EPSOUT))

(princ "\n[EPS_EXPORT 로드 완료]  명령어: EPSOUT  또는  EPSEXPORT")
(princ)
