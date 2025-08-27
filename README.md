# Daily Typing Practice

하루를 집중하면서 시작할 수 있도록 돕는 타자 연습 웹앱입니다.

마커스 아우렐리우스의 명상록 텍스트를 활용하여 타이핑 연습과 동시에 마음의 평정을 얻을 수 있습니다.

## 🌟 특징

- **명상록 기반**: 마커스 아우렐리우스의 명상록 짧은 구절들을 활용
- **집중력 향상**: 오타 발생 시 진행을 멈추고 정타 입력까지 대기
- **실시간 피드백**: 정타(초록), 교정된 오타(노랑), 현재 위치(파랑) 색상 표시
- **대소문자 무관**: 대문자/소문자 구분 없이 정타로 인정
- **정확한 측정**: WPM(분당 단어 수)과 정확도 측정
- **키보드 친화적**: Enter 키로 다음 구절 시작, 수식 키 무시
- **반응형 디자인**: 모바일과 데스크톱 모두 지원

## 🚀 사용법

### 온라인으로 사용
[GitHub Pages에서 바로 사용하기](https://hatemogi.github.io/daily-typing-webapp/)

### 로컬에서 실행
```bash
# 저장소 클론
git clone https://github.com/hatemogi/daily-typing-webapp.git
cd daily-typing-webapp

# 개발 서버 시작
make dev
```

브라우저에서 http://localhost:8000 접속

## 🛠️ 개발

### 필요한 도구
- [Elm](https://elm-lang.org/) - 함수형 프로그래밍 언어
- Python 3 - 로컬 개발 서버용
- Make - 빌드 자동화

### Make 명령어
```bash
make build  # Elm 컴파일
make serve  # 로컬 서버 실행
make dev    # 빌드 후 서버 실행
make clean  # 생성된 파일 정리
```

### 기술 스택
- **Frontend**: Elm, HTML5, CSS3
- **배포**: GitHub Pages + GitHub Actions
- **데이터**: JSON (명상록 텍스트)

## 📝 사용 방법

1. **시작**: 페이지가 로드되면 자동으로 타이핑 영역에 포커스
2. **타이핑**: 표시된 지문을 보고 키보드로 입력
3. **집중 모드**: 오타가 발생하면 정타를 입력할 때까지 진행 안 됨
4. **완료**: 구절을 완료하면 WPM과 정확도 표시
5. **다음 구절**: Enter 키를 눌러 새로운 구절로 이동

## 🎯 교육적 가치

- **타이핑 실력 향상**: 정확하고 빠른 타이핑 연습
- **집중력 개발**: 오류 발생 시 즉시 교정하는 습관
- **철학적 사고**: 명상록을 통한 인문학적 소양
- **꾸준한 습관**: 하루를 시작하는 의미 있는 루틴

## 📊 측정 지표

- **진행률**: 현재 구절의 완성도 (%)
- **실수 횟수**: 오타 발생 횟수
- **WPM**: 분당 타이핑 단어 수 (완료 시 고정)
- **정확도**: 전체 타이핑 중 정확한 문자 비율 (%)

## 🤝 기여하기

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📜 라이센스

이 프로젝트는 MIT 라이센스 하에 배포됩니다. 자세한 내용은 `LICENSE` 파일을 참조하세요.

## 🙏 감사의 말

- **마커스 아우렐리우스**: 영감을 주는 명상록 텍스트
- **Elm 커뮤니티**: 안정적이고 즐거운 개발 경험
- **모든 기여자들**: 프로젝트 개선에 도움을 주신 분들

---

*"모든 순간을 로마인으로서, 인간으로서 완벽하고 단순한 품위와 애정, 자유, 정의로써 행하라"* - 마커스 아우렐리우스