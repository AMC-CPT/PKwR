# ACRE 문법 강조 파일

[ACRE](https://github.com/AMC-CPT/ACRE)(Asan Clinical Research Editor)가 쓰는 문법 강조 정의
파일이다. 이 책 1장에서 편집기를 소개하므로 독자가 곧바로 쓸 수 있도록 여기에 함께 둔다.
원본은 ACRE 의 소스 저장소(비공개) `preset/Syntax/` 에 있고, 이 폴더의 파일은 그 사본이다.

| 파일 | 대상 |
|---|---|
| `Syntax/NONMEM.stx` | NONMEM 제어파일 (`*.CTL`, `*.MOD`, `*.CDT`) |
| `Syntax/R.stx`, `Syntax/Rd.stx` | R 스크립트와 R 문서 |
| `Syntax/LaTeX.stx` | LaTeX 원고 |
| `Syntax/C.stx`, `Cpp.stx`, `CSharp.stx`, `Fortran.stx`, `Python.stx`, `JavaScript.stx`, `HTML.stx`, `PDF.stx` | 그 밖의 언어 |

## 쓰는 법

ACRE 를 설치하면 같은 파일이 `C:\Program Files\ACRE\Syntax\` 에 들어간다. 정의를 고쳐 쓰거나
설치본이 오래되었을 때 이 폴더의 파일을 그 자리에 복사하면 된다. ACRE 를 다시 시작하면 적용된다.

파일은 `#키=값` 꼴의 평문이므로 편집기로 직접 고칠 수 있다. `#Extensions` 는 이 정의가 붙을
확장자, `#KeyWords1`~ 은 강조할 낱말 목록, `#...Color` 는 색이다.

라이선스는 ACRE 와 같은 MIT 이다.
