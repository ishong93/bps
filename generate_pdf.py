"""ATMS_explained.md → PDF 변환 스크립트 (reportlab)."""
import os
import subprocess
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import mm
from reportlab.lib.colors import HexColor, white, black
from reportlab.lib.styles import ParagraphStyle
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
    PageBreak, HRFlowable
)
from reportlab.lib.enums import TA_LEFT, TA_CENTER

# ── 한글 폰트 탐색 ──
FONT_CANDIDATES = [
    ("/usr/share/fonts/truetype/nanum/NanumGothic.ttf",
     "/usr/share/fonts/truetype/nanum/NanumGothicBold.ttf"),
    ("/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.ttc",
     "/usr/share/fonts/truetype/noto/NotoSansCJK-Bold.ttc"),
]

font_regular = None
font_bold = None

for reg, bld in FONT_CANDIDATES:
    if os.path.exists(reg):
        font_regular = reg
        font_bold = bld if os.path.exists(bld) else reg
        break

if not font_regular:
    # 시스템에서 CJK 폰트 검색
    result = subprocess.run(
        ["find", "/usr/share/fonts", "-name", "*.ttf"],
        capture_output=True, text=True
    )
    cjk = [f for f in result.stdout.strip().split("\n")
           if any(k in f.lower() for k in ["nanum", "noto", "gothic"])]
    if cjk:
        font_regular = cjk[0]
        bold_cands = [f for f in cjk if "bold" in f.lower()]
        font_bold = bold_cands[0] if bold_cands else font_regular

from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

FONT_NAME = "KR"
FONT_NAME_B = "KRB"

if font_regular:
    pdfmetrics.registerFont(TTFont(FONT_NAME, font_regular))
    pdfmetrics.registerFont(TTFont(FONT_NAME_B, font_bold))
else:
    FONT_NAME = "Helvetica"
    FONT_NAME_B = "Helvetica-Bold"

# ── 스타일 정의 ──
NAVY = HexColor("#1E3C78")
ACCENT = HexColor("#C83C32")

style_title = ParagraphStyle("Title", fontName=FONT_NAME_B, fontSize=28,
                              alignment=TA_CENTER, spaceAfter=5)
style_subtitle = ParagraphStyle("Subtitle", fontName=FONT_NAME, fontSize=14,
                                 alignment=TA_CENTER, spaceAfter=20,
                                 textColor=HexColor("#666666"))
style_h1 = ParagraphStyle("H1", fontName=FONT_NAME_B, fontSize=18,
                            spaceAfter=8, spaceBefore=16,
                            textColor=white, backColor=NAVY,
                            leftIndent=6, borderPadding=(6, 8, 6, 8))
style_h2 = ParagraphStyle("H2", fontName=FONT_NAME_B, fontSize=13,
                            spaceAfter=6, spaceBefore=12,
                            textColor=NAVY)
style_body = ParagraphStyle("Body", fontName=FONT_NAME, fontSize=10,
                              spaceAfter=4, leading=15)
style_code = ParagraphStyle("Code", fontName="Courier", fontSize=9,
                              spaceAfter=4, leading=12,
                              backColor=HexColor("#F0F0F0"),
                              leftIndent=10, borderPadding=(4, 6, 4, 6))
style_quote = ParagraphStyle("Quote", fontName=FONT_NAME_B, fontSize=11,
                               spaceAfter=6, leading=16,
                               backColor=HexColor("#FFF5DC"),
                               leftIndent=10, borderPadding=(8, 10, 8, 10),
                               textColor=NAVY)
style_small = ParagraphStyle("Small", fontName=FONT_NAME, fontSize=9,
                               alignment=TA_CENTER, textColor=HexColor("#999999"))

# ── 테이블 스타일 ──
def make_table(data, col_widths=None):
    t = Table(data, colWidths=col_widths, hAlign='LEFT')
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), NAVY),
        ('TEXTCOLOR', (0, 0), (-1, 0), white),
        ('FONTNAME', (0, 0), (-1, 0), FONT_NAME_B),
        ('FONTNAME', (0, 1), (-1, -1), FONT_NAME),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('GRID', (0, 0), (-1, -1), 0.5, HexColor("#CCCCCC")),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [white, HexColor("#F8F8FF")]),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('LEFTPADDING', (0, 0), (-1, -1), 6),
        ('RIGHTPADDING', (0, 0), (-1, -1), 6),
        ('TOPPADDING', (0, 0), (-1, -1), 4),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 4),
    ]))
    return t

# ── PDF 생성 ──
output_path = "/home/user/bps/ATMS_explained.pdf"
doc = SimpleDocTemplate(output_path, pagesize=A4,
                         topMargin=20*mm, bottomMargin=20*mm,
                         leftMargin=18*mm, rightMargin=18*mm)

story = []

# 표지
story.append(Spacer(1, 50*mm))
story.append(Paragraph("ATMS", style_title))
story.append(Paragraph("Assumption-based Truth Maintenance System", style_subtitle))
story.append(Spacer(1, 10*mm))
story.append(Paragraph("목적  |  해결 과제  |  방식  |  의의",
                         ParagraphStyle("Sub2", fontName=FONT_NAME_B, fontSize=16,
                                        alignment=TA_CENTER)))
story.append(Spacer(1, 30*mm))
story.append(Paragraph("원본: Building Problem Solvers", style_small))
story.append(Paragraph("Kenneth D. Forbus &amp; Johan de Kleer, 1993", style_small))
story.append(Paragraph("코드: atms.lisp v61, 1992-07-21", style_small))

# ── 1장 ──
story.append(PageBreak())
story.append(Paragraph("1. 탄생 배경: '한 번에 하나의 세계'의 한계", style_h1))
story.append(Paragraph(
    "1986년, Xerox PARC의 <b>Johan de Kleer</b>는 기존 추론 시스템(JTMS)의 "
    "근본적 한계에 직면했습니다.", style_body))
story.append(Paragraph(
    "JTMS는 각 명제에 <b>IN 또는 OUT</b>, 단 하나의 레이블만 부여합니다. "
    "한 시점에 오직 하나의 세계관만 유지할 수 있었습니다.", style_body))
story.append(Paragraph(
    'JTMS: "지금 이 순간, X는 참이다 / 거짓이다"\n'
    '      → 다른 가능성을 보려면 가정을 철회하고 다시 추론', style_code))
story.append(Paragraph(
    "이것은 <b>진단(diagnosis)</b> 같은 문제에서 치명적이었습니다:", style_body))
story.append(Paragraph(
    "환자에게 증상 A, B, C가 있다. 원인이 질병1일 수도, 질병2일 수도 있다.\n"
    'JTMS: "질병1" 가정 → 추론 → 안 맞음 → 철회 → "질병2" 가정 → 추론...\n'
    "매번 처음부터 다시!", style_code))
story.append(Paragraph(
    'de Kleer의 질문: "모든 가능한 세계를 동시에 추적할 수는 없을까?"', style_quote))
story.append(Paragraph("이 질문의 답이 바로 <b>ATMS</b>입니다.", style_body))

# ── 2장 ──
story.append(Paragraph("2. 해결 과제", style_h1))
story.append(Paragraph("2.1 다중 세계 동시 추적", style_h2))
story.append(Paragraph(
    "JTMS:  노드.label = :IN 또는 :OUT          (하나의 답)\n"
    "ATMS:  노드.label = [{A}, {B,C}]           (여러 세계에서 참)", style_code))

story.append(Paragraph("2.2 모순 발견 시 자동 격리", style_h2))
story.append(Paragraph(
    "JTMS는 모순을 만나면 멈추지만, ATMS는 모순 환경만 nogood으로 표시하고 "
    "나머지 세계는 계속 작동합니다.", style_body))
story.append(Paragraph(
    "{A,B} 모순 → E-5:* {A,B} 표시\n"
    "{A}, {B}, {C}, {B,C} 세계는 계속 유효!", style_code))

story.append(Paragraph("2.3 추론 결과의 재사용", style_h2))
story.append(Paragraph(
    "JTMS는 가정을 바꿀 때마다 모든 추론을 처음부터 해야 합니다. "
    "ATMS는 한 번 계산한 추론 결과를 모든 세계에서 공유합니다.", style_body))

# ── 3장 ──
story.append(PageBreak())
story.append(Paragraph("3. 방식: 환경 기반 레이블 관리", style_h1))
story.append(Paragraph(
    '"노드의 참/거짓을 묻지 말고, 어떤 가정 하에서 참인지를 물어라"', style_quote))

story.append(Paragraph("3.1 아키텍처 구성 요소", style_h2))
story.append(make_table([
    ["구성 요소", "설명"],
    ["가정(Assumption)", "독립적 선택지. 참/거짓을 자유롭게 설정 가능"],
    ["환경(Environment)", "가정들의 집합 = 하나의 '가능한 세계'. 예: {A, C}"],
    ["레이블(Label)", "최소 환경들의 antichain. 예: [{A}, {B,C}]"],
    ["Nogood", "모순인 환경. {A,B}* = A와 B는 동시에 참일 수 없다"],
    ["Justification", "추론 규칙. antecedents → consequence"],
], col_widths=[35*mm, 135*mm]))

story.append(Spacer(1, 5*mm))
story.append(Paragraph("3.2 JTMS vs ATMS 비교", style_h2))
story.append(make_table([
    ["측면", "JTMS", "ATMS"],
    ["노드 레이블", ":IN / :OUT", "환경 리스트 [{A}, {B,C}]"],
    ["동시 세계", "1개", "모든 일관된 세계"],
    ["모순 처리", "멈추고 철회", "nogood 표시 → 자동 격리"],
    ["가정 변경", "철회 → 재추론", "이미 계산되어 있음"],
    ["질문 방식", '"X는 참인가?"', '"어떤 가정 하에서 참인가?"'],
    ["핵심 연산", "enable/retract", "weave (교차곱 + 필터링)"],
], col_widths=[30*mm, 55*mm, 85*mm]))

story.append(Spacer(1, 5*mm))
story.append(Paragraph("3.3 new-nogood의 5단계", style_h2))
story.append(Paragraph(
    "① 금지 도장    → 이 환경은 모순이다\n"
    "② label 제거   → 모순 세계에서 참은 무의미\n"
    "③ nogood-table → 중앙 목록에 등록\n"
    "④ 상위 정리    → 큰 금지는 작은 게 커버 → 중복 제거\n"
    "⑤ 상향 오염    → 유효한 상위 환경도 즉시 오염", style_code))

# ── 4장 ──
story.append(PageBreak())
story.append(Paragraph("4. step-1 예제: ATMS의 진가", style_h1))
story.append(Paragraph("de Kleer 논문의 예제 (atest.lisp:43-80)", style_body))
story.append(Paragraph(
    "초기:  A, B, C (가정)\n"
    "       J1: A → x=1      x=1.label = [{A}]\n"
    "       J2: B → y=x      y=x.label = [{B}]\n"
    "       J3: C → x=z      x=z.label = [{C}]\n"
    "\n"
    "모순:  nogood({A, B})    → {A,B}* 금지!\n"
    "\n"
    "시도:  J4: x=1 + y=x → y=1\n"
    "       {A} x {B} = {A,B} → nogood! 차단!\n"
    "       y=1.label = []     ← 증명 불가\n"
    "\n"
    "전환:  Premise: z=1       z=1.label = [{∅}]\n"
    "       J5: z=1 + x=z → x=1\n"
    "       x=1.label = [{A}, {C}]  ← 두 경로!\n"
    "\n"
    "재시도: J4 재전파\n"
    "       {C} x {B} = {B,C} → 유효!\n"
    "       y=1.label = [{B,C}]  ← 우회 성공!", style_code))
story.append(Paragraph(
    "JTMS였다면: {A,B} 모순 → A 철회 → 모든 추론 재시작", style_body))
story.append(Paragraph(
    "<b>ATMS는: {A,B}만 격리, {C} 경로를 자동으로 찾아냄! 재계산 없음!</b>", style_body))

# ── 5장 ──
story.append(PageBreak())
story.append(Paragraph("5. 의의", style_h1))

story.append(Paragraph("5.1 패러다임 전환", style_h2))
story.append(Paragraph(
    'JTMS: "X는 참이다"\n'
    'ATMS: "X는 {A} 하에서 참이고, {B,C} 하에서도 참이다"', style_code))

story.append(Paragraph("5.2 실용적 응용 분야", style_h2))
story.append(make_table([
    ["분야", "ATMS 활용"],
    ["의료 진단", "여러 질병 가설을 동시에 추적, 검사 결과로 가설 제거"],
    ["회로 진단", "고장 부품 후보를 병렬 추적 (de Kleer의 원래 목적)"],
    ["설계 탐색", "여러 설계 대안을 동시에 유지, 제약 위반 시 자동 격리"],
    ["계획 수립", "여러 행동 계획을 병렬 평가, 실패 시 대안 자동 활성화"],
], col_widths=[35*mm, 135*mm]))

story.append(Spacer(1, 5*mm))
story.append(Paragraph("5.3 de Kleer의 핵심 설계 원칙", style_h2))
story.append(Paragraph(
    "1. <b>발생 시점 처리</b>: 매번 전수검사 대신, 발생 시 한 번에 전파\n"
    "2. <b>최소성 유지</b>: 테이블에 최소 집합만 → 메모리+속도 최적화\n"
    "3. <b>부분집합 판정</b>: {A,B} ⊆ X 관계로만 오염 → 정확하고 안전\n"
    "4. <b>역참조 활용</b>: env.nodes로 O(k) 빠른 label 정리\n"
    "5. <b>이유 기록</b>: '왜 금지?'를 env에 저장 → 설명 생성 가능", style_body))

story.append(Spacer(1, 10*mm))
story.append(Paragraph(
    'ATMS는 "여러 가능한 세계를 동시에 추적하면서, '
    '모순이 발견되면 해당 세계만 격리하고 '
    '나머지는 계속 작동하게 하는" 진리 유지 시스템이다.', style_quote))

# ── 빌드 ──
doc.build(story)
print(f"PDF 생성 완료: {output_path}")
