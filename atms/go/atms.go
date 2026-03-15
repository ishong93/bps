// 가정 기반 진리 유지 시스템 (Assumption-based Truth Maintenance System)
// 버전 61, 1992년 7월 21일.
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Common Lisp에서 Go로 변환됨.
//
// ATMS는 JTMS(정당화 기반 TMS)와 달리 여러 가정 집합(환경)을 동시에 관리한다.
// 각 노드는 그 노드를 참으로 만드는 최소 환경들의 목록(레이블)을 유지하며,
// 모순이 발견되면 해당 환경을 nogood로 표시하여 전파한다.

package atms

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
)

// EnvCompareResult는 두 환경을 비교한 결과를 나타내는 열거형이다.
// 두 환경 간의 부분집합 관계를 표현한다.
type EnvCompareResult int

const (
	CompareEQ  EnvCompareResult = iota // 두 환경이 동일함
	CompareS12                         // e1이 e2의 부분집합
	CompareS21                         // e2가 e1의 부분집합
	CompareNone                        // 부분집합 관계 없음
)

// EnvTableEntry는 환경 테이블(env-table) 또는 nogood 테이블의 한 버킷을 나타낸다.
// 가정의 개수(Count)를 키로 사용하여, 같은 크기의 환경들을 묶어 관리한다.
// 이 구조를 통해 포섭(subsumption) 검사 시 작은 환경부터 순서대로 탐색할 수 있다.
type EnvTableEntry struct {
	Count int    // 이 버킷에 속한 환경들의 가정 개수
	Envs  []*Env // 해당 개수의 가정을 가진 환경 목록
}

// ATMS는 가정 기반 진리 유지 시스템의 핵심 구조체이다.
// 모든 노드, 정당화, 환경, nogood 정보를 중앙에서 관리한다.
//
// JTMS와의 주요 차이점:
// - JTMS는 한 번에 하나의 가정 집합만 유지하지만, ATMS는 모든 가능한 가정 조합을 동시에 추적한다.
// - 각 노드는 단일 진리값 대신, 그 노드를 참으로 만드는 최소 환경들의 목록(레이블)을 가진다.
type ATMS struct {
	Title            string                     // ATMS 인스턴스의 이름
	NodeCounter      int                        // 노드 고유 ID 생성용 카운터
	JustCounter      int                        // 정당화 고유 ID 생성용 카운터
	EnvCounter       int                        // 환경 고유 ID 생성용 카운터
	Nodes            []*TmsNode                 // 시스템에 등록된 모든 노드 목록
	Justs            []*Just                    // 시스템에 등록된 모든 정당화 목록
	Contradictions   []*TmsNode                 // 모순으로 표시된 노드 목록
	Assumptions      []*TmsNode                 // 가정으로 지정된 노드 목록
	Debugging        bool                       // 디버그 메시지 출력 여부
	NogoodTable      []EnvTableEntry            // 모순(nogood) 환경 테이블 - 가정 개수별로 정렬
	ContraNode       *TmsNode                   // 모순을 나타내는 특수 노드 (모든 nogood 정당화의 귀결)
	EnvTable         []EnvTableEntry            // 전체 환경 테이블 - 가정 개수별로 정렬
	EmptyEnv         *Env                       // 빈 환경 (가정 없음) - 무조건 참인 사실의 기반
	NodeString       func(*TmsNode) string      // 노드를 문자열로 변환하는 사용자 정의 함수
	EnqueueProcedure func(interface{})          // 규칙 실행을 위한 큐잉 프로시저 (추론 엔진 연동용)
}

// String은 ATMS의 문자열 표현을 반환한다. (fmt.Stringer 인터페이스 구현)
func (a *ATMS) String() string {
	return fmt.Sprintf("#<ATMS: %s>", a.Title)
}

// TmsNode는 ATMS에서 하나의 명제(proposition)를 나타내는 노드이다.
// 각 노드는 자신을 참으로 만드는 최소 환경들의 목록(Label)을 유지한다.
//
// Label은 최소 환경만 포함한다: 만약 환경 E1이 E2의 부분집합이면,
// E2는 레이블에서 제거된다 (E1만으로 충분하므로).
type TmsNode struct {
	Index          int            // 노드의 고유 인덱스 번호
	Datum          interface{}    // 노드에 연결된 외부 데이터 (추론 엔진의 사실/명제)
	Label          []*Env         // 이 노드를 참으로 만드는 최소 환경들의 목록
	Justs          []*Just        // 이 노드를 귀결(consequence)로 가지는 정당화 목록
	Consequences   []*Just        // 이 노드를 선행조건(antecedent)으로 사용하는 정당화 목록
	Contradictory  bool           // true이면 이 노드가 참인 환경은 모두 nogood가 됨
	IsAssumption   bool           // true이면 이 노드는 가정이며, 자체 단일 환경을 가짐
	Rules          []interface{}  // 이 노드의 레이블이 갱신될 때 실행할 규칙 목록
	ATMS           *ATMS          // 이 노드가 속한 ATMS 인스턴스
}

// String은 노드의 문자열 표현을 반환한다. (fmt.Stringer 인터페이스 구현)
// 가정 노드는 "A-인덱스" 형식으로, 일반 노드는 NodeString 함수를 사용한다.
func (n *TmsNode) String() string {
	if n.IsAssumption {
		return fmt.Sprintf("A-%d", n.Index)
	}
	return fmt.Sprintf("#<NODE: %s>", NodeString(n))
}

// Just는 ATMS에서의 정당화(justification)를 나타낸다.
// 정당화는 "선행조건 노드들이 모두 참이면 귀결 노드도 참이다"라는 추론 규칙이다.
//
// 예: Informant="규칙1", Antecedents=[A, B], Consequence=C 이면
//     "A와 B가 모두 참이면 C도 참이다 (규칙1에 의해)"를 의미한다.
type Just struct {
	Index       int            // 정당화의 고유 인덱스 번호
	Informant   interface{}    // 이 정당화를 생성한 출처 (규칙 이름, 추론 엔진 식별자 등)
	Consequence *TmsNode       // 귀결 노드 - 선행조건이 충족되면 참이 되는 노드
	Antecedents []*TmsNode     // 선행조건 노드 목록 - 모두 참이어야 귀결이 성립
}

// String은 정당화의 문자열 표현을 반환한다. (fmt.Stringer 인터페이스 구현)
func (j *Just) String() string {
	return fmt.Sprintf("<%v %d>", j.Informant, j.Index)
}

// Env는 환경(environment)을 나타낸다.
// 환경은 가정(assumption) 노드들의 집합으로, 하나의 가능한 세계관을 표현한다.
//
// ATMS에서 환경은 핵심 개념이다:
// - 각 노드의 레이블은 그 노드를 참으로 만드는 환경들의 목록이다.
// - 환경이 nogood로 표시되면, 해당 가정 조합은 모순적이라는 의미이다.
// - 환경의 합집합(union)은 두 사실을 동시에 참으로 만드는 가정 집합이다.
type Env struct {
	Index       int            // 환경의 고유 인덱스 번호
	Count       int            // 이 환경에 포함된 가정의 개수 (빠른 비교용 캐시)
	Assumptions []*TmsNode     // 이 환경을 구성하는 가정 노드들 (인덱스 순으로 정렬)
	Nodes       []*TmsNode     // 이 환경을 레이블에 포함하는 노드 목록 (역참조용)
	Nogood      interface{}    // nil이면 유효, non-nil이면 모순 (모순의 원인: *Just 또는 *Env)
	Rules       []interface{}  // 이 환경이 제거될 때 실행할 규칙 목록
}

// IsNogood는 이 환경이 모순(nogood)으로 표시되었는지 확인한다.
// Nogood 필드가 nil이 아니면, 이 가정 조합은 일관성이 없다는 의미이다.
func (e *Env) IsNogood() bool {
	return e.Nogood != nil
}

// String은 환경의 문자열 표현을 반환한다. (fmt.Stringer 인터페이스 구현)
func (e *Env) String() string {
	return fmt.Sprintf("E-%d", e.Index)
}

// ============================================================================
// 기본 유틸리티 함수들
// ============================================================================

// NodeString은 ATMS에 등록된 node-string 함수를 사용하여 노드를 문자열로 변환한다.
// 외부 추론 엔진이 제공하는 표현 방식으로 노드를 출력한다.
func NodeString(node *TmsNode) string {
	return node.ATMS.NodeString(node)
}

// debugging은 ATMS의 디버깅 모드가 활성화된 경우에만 디버그 메시지를 표준 에러에 출력한다.
func debugging(atms *ATMS, msg string, args ...interface{}) {
	if atms.Debugging {
		fmt.Fprintf(os.Stderr, msg, args...)
	}
}

// DefaultNodeString은 노드를 문자열로 변환하는 기본 함수이다.
// 노드의 Datum 필드를 그대로 문자열로 변환한다.
func DefaultNodeString(n *TmsNode) string {
	return fmt.Sprintf("%v", n.Datum)
}

// OrderedInsert는 정렬된 슬라이스에 항목을 삽입하고 정렬 순서를 유지한다.
// less 함수로 정렬 기준을 지정한다. 이미 존재하는 항목(포인터 동일)이면 원본 슬라이스를 그대로 반환한다.
// 이 함수는 환경의 가정 목록을 인덱스 순으로 유지하는 데 사용된다.
func OrderedInsert(item *TmsNode, list []*TmsNode, less func(*TmsNode, *TmsNode) bool) []*TmsNode {
	// 빈 리스트면 항목 하나만 담은 새 슬라이스 반환
	if len(list) == 0 {
		return []*TmsNode{item}
	}
	// 첫 번째 요소보다 앞에 와야 하면 맨 앞에 삽입
	if less(item, list[0]) {
		return append([]*TmsNode{item}, list...)
	}
	// 이미 존재하면 변경 없이 반환 (중복 방지)
	if item == list[0] {
		return list
	}
	// 재귀적으로 나머지 리스트에서 적절한 위치를 찾아 삽입
	rest := OrderedInsert(item, list[1:], less)
	// 나머지가 변경되지 않았으면 원본 리스트를 그대로 반환 (메모리 효율)
	if len(rest) == len(list[1:]) && len(rest) > 0 && &rest[0] == &list[1] {
		return list
	}
	result := make([]*TmsNode, 0, 1+len(rest))
	result = append(result, list[0])
	result = append(result, rest...)
	return result
}

// AssumptionOrder는 가정 노드의 정렬 기준 함수이다.
// 인덱스 번호가 작은 노드가 앞에 온다.
func AssumptionOrder(a1, a2 *TmsNode) bool {
	return a1.Index < a2.Index
}

// EnvOrder는 환경의 정렬 기준 함수이다.
// 인덱스 번호가 작은 환경이 앞에 온다.
func EnvOrder(e1, e2 *Env) bool {
	return e1.Index < e2.Index
}

// ============================================================================
// ATMS 생성 및 수정
// ============================================================================

// CreateATMS는 새로운 ATMS 인스턴스를 생성한다.
// 옵션 함수 패턴을 사용하여 NodeString, EnqueueProcedure, Debugging 등을 설정할 수 있다.
//
// 생성 시 자동으로:
// 1. 모순 노드(ContraNode)를 생성 - 모든 nogood 정당화의 귀결 노드 역할
// 2. 빈 환경(EmptyEnv)을 생성 - 가정 없이 참인 사실의 기반
func CreateATMS(title string, opts ...func(*ATMS)) *ATMS {
	atms := &ATMS{
		Title:      title,
		NodeString: DefaultNodeString,
	}
	// 사용자 지정 옵션 적용
	for _, opt := range opts {
		opt(atms)
	}
	if atms.NodeString == nil {
		atms.NodeString = DefaultNodeString
	}
	// 모순 전용 노드 생성: 이 노드가 참이 되는 환경은 자동으로 nogood 처리됨
	atms.ContraNode = TmsCreateNode(atms, "The contradiction", TMSNodeOpts{Contradictory: true})
	// 빈 환경 생성: 무조건 참인 사실(정리, 공리 등)은 빈 환경에서 성립
	atms.EmptyEnv = CreateEnv(atms, nil)
	return atms
}

// TMSNodeOpts는 TmsCreateNode의 선택적 매개변수를 담는 구조체이다.
type TMSNodeOpts struct {
	IsAssumption  bool // true이면 가정 노드로 생성
	Contradictory bool // true이면 모순 노드로 생성 (이 노드가 참인 환경은 nogood)
}

// ChangeATMS는 ATMS의 속성을 동적으로 변경한다.
// nil이 아닌 매개변수만 업데이트된다.
func ChangeATMS(atms *ATMS, nodeString func(*TmsNode) string, enqueueProcedure func(interface{}), debugging *bool) {
	if nodeString != nil {
		atms.NodeString = nodeString
	}
	if debugging != nil {
		atms.Debugging = *debugging
	}
	if enqueueProcedure != nil {
		atms.EnqueueProcedure = enqueueProcedure
	}
}

// ============================================================================
// 노드 상태 질의 함수들
// ============================================================================

// TrueNode는 노드가 무조건 참(빈 환경에서 성립)인지 확인한다.
// 빈 환경이 레이블의 첫 번째에 있으면, 어떤 가정도 없이 참이라는 의미이다.
// 이는 해당 명제가 정리(theorem)임을 뜻한다.
func TrueNode(node *TmsNode) bool {
	if len(node.Label) == 0 {
		return false
	}
	return node.Label[0] == node.ATMS.EmptyEnv
}

// InNode는 노드가 주어진 환경에서 "참(in)"인지 확인한다.
//
// env가 nil이면: 레이블이 비어있지 않은지 확인 (어떤 환경에서든 참인지)
// env가 non-nil이면: 레이블 중 하나라도 env의 부분집합인지 확인
//
// 노드의 레이블 환경 E가 env의 부분집합이면, env에서 노드가 참이다.
// (env는 E의 모든 가정을 포함하므로)
func InNode(n *TmsNode, env *Env) bool {
	if env != nil {
		// 레이블의 각 환경이 주어진 환경의 부분집합인지 확인
		for _, le := range n.Label {
			if SubsetEnv(le, env) {
				return true
			}
		}
		return false
	}
	// env가 nil이면 레이블이 비어있지 않은지만 확인
	return len(n.Label) > 0
}

// OutNode는 노드가 주어진 환경에서 "참이 아닌(out)" 상태인지 확인한다.
// InNode의 반대이다.
func OutNode(n *TmsNode, env *Env) bool {
	return !InNode(n, env)
}

// NodeConsistentWith는 노드가 주어진 환경과 일관성이 있는지 확인한다.
// 노드의 레이블 환경 중 하나라도 env와 합쳤을 때 nogood가 아니면 일관적이다.
// 이는 노드가 env의 가정들과 동시에 참일 수 있는지를 판단한다.
func NodeConsistentWith(n *TmsNode, env *Env) bool {
	for _, le := range n.Label {
		u := UnionEnv(le, env)
		if u != nil && !u.IsNogood() {
			return true
		}
	}
	return false
}

// ============================================================================
// 노드 생성 및 변환
// ============================================================================

// TmsCreateNode는 ATMS에 새로운 노드를 생성한다.
//
// 일반 노드: 레이블이 비어있는 상태로 생성 (아직 참이 아님)
// 가정 노드(IsAssumption=true): 자신만을 포함하는 환경을 생성하여 레이블에 추가
// 모순 노드(Contradictory=true): 이 노드가 참이 되는 환경은 자동으로 nogood 처리
func TmsCreateNode(atms *ATMS, datum interface{}, opts TMSNodeOpts) *TmsNode {
	atms.NodeCounter++
	node := &TmsNode{
		Index:         atms.NodeCounter,
		Datum:         datum,
		IsAssumption:  opts.IsAssumption,
		Contradictory: opts.Contradictory,
		ATMS:          atms,
	}
	// 노드 목록의 앞에 추가 (최신 노드가 앞에 옴)
	atms.Nodes = append([]*TmsNode{node}, atms.Nodes...)
	// 모순 노드면 모순 목록에 추가
	if opts.Contradictory {
		atms.Contradictions = append([]*TmsNode{node}, atms.Contradictions...)
	}
	// 가정 노드면 자신만의 환경을 생성하고 레이블에 설정
	if opts.IsAssumption {
		atms.Assumptions = append([]*TmsNode{node}, atms.Assumptions...)
		// {node} 환경 생성: "node가 참이라고 가정하면 node는 참이다"
		env := CreateEnv(atms, []*TmsNode{node})
		node.Label = append([]*Env{env}, node.Label...)
	}
	return node
}

// AssumeNode는 기존 노드를 가정(assumption)으로 변환한다.
// 이미 가정인 노드에는 아무 작업도 하지 않는다.
//
// 변환 과정:
// 1. 노드를 가정으로 표시
// 2. 가정 목록에 추가
// 3. 자신만의 환경을 생성
// 4. 해당 환경을 레이블에 전파 (Update 호출)
func AssumeNode(node *TmsNode) {
	if node.IsAssumption {
		return
	}
	atms := node.ATMS
	debugging(atms, "\nConverting %s into an assumption", NodeString(node))
	node.IsAssumption = true
	atms.Assumptions = append([]*TmsNode{node}, atms.Assumptions...)
	env := CreateEnv(atms, []*TmsNode{node})
	// Update를 통해 새 환경을 노드의 레이블에 추가하고, 후속 정당화에 전파
	Update([]*Env{env}, node, "ASSUME-NODE")
}

// MakeContradiction은 노드를 모순 노드로 표시한다.
// 이 노드의 레이블에 있는 모든 환경을 nogood로 처리한다.
//
// 사용 예: 진단 추론에서 "A와 B가 동시에 참이면 모순"을 나타낼 때,
// 모순 노드의 선행조건을 A와 B로 설정하면 {A, B} 환경이 nogood가 된다.
func MakeContradiction(node *TmsNode) {
	atms := node.ATMS
	if node.Contradictory {
		return
	}
	node.Contradictory = true
	atms.Contradictions = append([]*TmsNode{node}, atms.Contradictions...)
	// 현재 레이블의 모든 환경을 nogood로 전환
	for {
		if len(node.Label) == 0 {
			break
		}
		nogood := node.Label[0]
		if nogood == nil {
			break
		}
		NewNogood(atms, nogood, "MAKE-CONTRADICTION")
	}
}

// ============================================================================
// 정당화(Justification) 생성
// ============================================================================

// JustifyNode는 새로운 정당화를 추가한다.
// "informant에 의해, antecedents가 모두 참이면 consequence도 참이다"를 등록한다.
//
// 정당화 추가 후:
// 1. 귀결 노드의 Justs 목록에 추가
// 2. 각 선행조건 노드의 Consequences 목록에 추가
// 3. 빈 환경을 시작으로 Propagate를 호출하여 레이블을 갱신
//
// 선행조건이 비어있으면 (antecedents=[]) 귀결은 무조건 참이 된다 (빈 환경에서 성립).
func JustifyNode(informant interface{}, consequence *TmsNode, antecedents []*TmsNode) *Just {
	atms := consequence.ATMS
	atms.JustCounter++
	just := &Just{
		Index:       atms.JustCounter,
		Informant:   informant,
		Consequence: consequence,
		Antecedents: antecedents,
	}
	// 귀결 노드에 이 정당화 등록
	consequence.Justs = append([]*Just{just}, consequence.Justs...)
	// 각 선행조건 노드에 역참조 등록 (이 노드가 변하면 이 정당화를 다시 평가)
	for _, node := range antecedents {
		node.Consequences = append([]*Just{just}, node.Consequences...)
	}
	atms.Justs = append([]*Just{just}, atms.Justs...)

	if atms.Debugging {
		antStrs := make([]string, len(antecedents))
		for i, a := range antecedents {
			antStrs[i] = NodeString(a)
		}
		debugging(atms, "\nJustifying %s in terms of %v on %v",
			NodeString(consequence), informant, antStrs)
	}

	// 빈 환경에서 시작하여 선행조건들의 레이블을 조합(weave)하고 귀결에 전파
	Propagate(just, nil, []*Env{atms.EmptyEnv})
	return just
}

// NogoodNodes는 주어진 노드들이 동시에 참일 수 없음(상호 모순)을 선언한다.
// 내부적으로 모순 노드(ContraNode)에 대한 정당화를 추가한다.
// 즉, "이 노드들이 모두 참이면 모순이다"를 등록한다.
func NogoodNodes(informant interface{}, nodes []*TmsNode) *Just {
	return JustifyNode(informant, nodes[0].ATMS.ContraNode, nodes)
}

// ============================================================================
// 레이블 갱신 (Label Updating) - ATMS의 핵심 알고리즘
// ============================================================================

// Propagate는 정당화를 통해 환경을 전파한다.
// 주어진 환경 목록(envs)을 선행조건들의 레이블과 조합(Weave)한 후,
// 결과 환경들을 귀결 노드에 Update한다.
//
// antecedent가 nil이 아니면 해당 노드의 레이블은 Weave에서 건너뛴다
// (이미 envs에 반영되어 있으므로).
func Propagate(just *Just, antecedent *TmsNode, envs []*Env) {
	// 선행조건들의 레이블을 조합하여 가능한 환경 목록을 생성
	newEnvs := Weave(antecedent, envs, just.Antecedents)
	if newEnvs != nil {
		// 귀결 노드의 레이블을 갱신하고 후속 전파
		Update(newEnvs, just.Consequence, just)
	}
}

// Update는 귀결 노드의 레이블에 새 환경들을 추가하고 후속 전파를 수행한다.
//
// 동작 과정:
// 1. 귀결이 모순 노드면: 새 환경들을 nogood로 등록
// 2. 아니면: 레이블을 갱신 (포섭되는 환경 제거)
// 3. 규칙 큐잉 (EnqueueProcedure가 있으면)
// 4. 후속 정당화(consequence.Consequences)에 재귀적 전파
//
// just 매개변수는 *Just 또는 string("ASSUME-NODE") 타입이다.
func Update(newEnvs []*Env, consequence *TmsNode, just interface{}) {
	atms := consequence.ATMS

	// 귀결이 모순 노드면, 새 환경들을 모두 nogood로 표시
	if consequence.Contradictory {
		for _, env := range newEnvs {
			NewNogood(atms, env, just)
		}
		return
	}

	// 레이블을 갱신: 새 환경을 추가하고 포섭되는 기존 환경을 제거
	newEnvs = UpdateLabel(consequence, newEnvs)
	if len(newEnvs) == 0 {
		return // 실제로 추가된 새 환경이 없으면 전파 불필요
	}

	// 규칙 큐잉: 추론 엔진이 이 노드에 대해 대기 중인 규칙을 실행하도록 알림
	if atms.EnqueueProcedure != nil {
		for _, rule := range consequence.Rules {
			atms.EnqueueProcedure(rule)
		}
		consequence.Rules = nil
	}

	// 후속 정당화에 전파: 이 노드를 선행조건으로 사용하는 정당화들을 재평가
	for _, supportedJust := range consequence.Consequences {
		Propagate(supportedJust, consequence, newEnvs)

		// 전파 중 nogood 발견으로 제거된 환경을 newEnvs에서도 필터링
		for i := range newEnvs {
			if newEnvs[i] != nil {
				found := false
				for _, le := range consequence.Label {
					if newEnvs[i] == le {
						found = true
						break
					}
				}
				if !found {
					newEnvs[i] = nil
				}
			}
		}
		newEnvs = deleteNilEnvs(newEnvs)
		if len(newEnvs) == 0 {
			return
		}
	}
}

// UpdateLabel은 노드의 레이블에 새 환경들을 추가하고, 포섭(subsumption) 관계를 관리한다.
// 실제로 추가된 새 환경 목록을 반환한다.
//
// 포섭 규칙:
// - 새 환경이 기존 환경과 동일하거나 기존의 상위집합이면: 새 환경을 무시 (이미 더 작은 것이 있음)
// - 새 환경이 기존 환경의 부분집합이면: 기존 환경을 제거 (새 것이 더 일반적)
func UpdateLabel(node *TmsNode, newEnvs []*Env) []*Env {
	envs := node.Label

	for i := range newEnvs {
		if newEnvs[i] == nil {
			continue
		}
		found := false
		for j := range envs {
			if envs[j] == nil || newEnvs[i] == nil {
				continue
			}
			cmp := CompareEnv(newEnvs[i], envs[j])
			switch cmp {
			case CompareEQ, CompareS21:
				// 새 환경이 기존과 같거나 기존의 상위집합: 새 환경은 불필요
				newEnvs[i] = nil
				found = true
			case CompareS12:
				// 새 환경이 기존의 부분집합: 기존 환경을 제거 (새 것이 더 최소)
				envs[j].Nodes = deleteNodeOnce(envs[j].Nodes, node)
				envs[j] = nil
			}
			if found {
				break
			}
		}
		// 포섭되지 않은 새 환경을 레이블에 추가
		if !found && newEnvs[i] != nil {
			envs = append(envs, newEnvs[i])
		}
	}

	// 실제 추가된 환경에 이 노드를 역참조로 등록
	newEnvs = deleteNilEnvs(newEnvs)
	for _, newEnv := range newEnvs {
		newEnv.Nodes = append([]*TmsNode{node}, newEnv.Nodes...)
	}
	node.Label = deleteNilEnvsFromSlice(envs)
	return newEnvs
}

// Weave는 선행조건 노드들의 레이블을 교차 곱(cross product)으로 조합한다.
// 여러 선행조건이 동시에 참인 환경을 계산하는 핵심 알고리즘이다.
//
// 예: 선행조건 A의 레이블이 [{a1}, {a2}], B의 레이블이 [{b1}]이면
//     Weave 결과는 [{a1,b1}, {a2,b1}] (nogood가 아닌 것만)
//
// antecedent가 nil이 아니면, 해당 노드의 레이블은 이미 envs에 반영되었으므로 건너뛴다.
// 조합 중 포섭 관계를 검사하여 최소 환경만 유지한다.
func Weave(antecedent *TmsNode, envs []*Env, antecedents []*TmsNode) []*Env {
	// 입력 슬라이스를 복사하여 원본 보호
	envs = copyEnvSlice(envs)

	// 각 선행조건 노드에 대해 환경 조합을 수행
	for _, node := range antecedents {
		if node == antecedent {
			continue // 이미 반영된 선행조건은 건너뜀
		}
		var newEnvs []*Env
		// 현재 환경 목록과 노드의 레이블을 조합
		for _, env := range envs {
			if env == nil {
				continue
			}
			for _, nodeEnv := range node.Label {
				// 두 환경의 합집합을 계산
				newEnv := UnionEnv(env, nodeEnv)
				if newEnv == nil || newEnv.IsNogood() {
					continue // 합집합이 nogood이면 건너뜀
				}
				// 기존 결과와의 포섭 관계 확인
				subsumed := false
				for k := range newEnvs {
					if newEnvs[k] == nil {
						continue
					}
					cmp := CompareEnv(newEnv, newEnvs[k])
					switch cmp {
					case CompareEQ, CompareS21:
						subsumed = true // 이미 더 작은 환경이 있음
					case CompareS12:
						newEnvs[k] = nil // 새 환경이 더 작으므로 기존 것 제거
					}
					if subsumed {
						break
					}
				}
				if !subsumed {
					newEnvs = append(newEnvs, newEnv)
				}
			}
		}
		envs = deleteNilEnvs(newEnvs)
		if len(envs) == 0 {
			return nil // 가능한 조합이 없음 (모두 nogood)
		}
	}
	return envs
}

// InAntecedent는 주어진 노드들이 모두 동시에 참일 수 있는지 확인한다.
// 즉, 이 노드들의 레이블을 조합했을 때 nogood가 아닌 환경이 존재하는지 판단한다.
func InAntecedent(nodes []*TmsNode) bool {
	if len(nodes) == 0 {
		return true
	}
	return WeaveQ(nodes[0].ATMS.EmptyEnv, nodes)
}

// WeaveQ는 주어진 환경을 확장하여 모든 노드를 포함할 수 있는지 존재 여부를 확인한다.
// Weave의 존재 판정 버전이다 (전체 결과를 계산하지 않고 가능 여부만 판단).
//
// 깊이 우선 탐색으로 각 노드의 레이블 환경을 하나씩 추가하며,
// nogood가 아닌 완전한 조합을 찾으면 true를 반환한다.
func WeaveQ(env *Env, nodes []*TmsNode) bool {
	if len(nodes) == 0 {
		return true // 모든 노드를 성공적으로 포함
	}
	for _, e := range nodes[0].Label {
		newEnv := UnionEnv(e, env)
		if newEnv == nil || newEnv.IsNogood() {
			continue
		}
		if WeaveQ(newEnv, nodes[1:]) {
			return true
		}
	}
	return false
}

// SupportingAntecedent는 모든 노드가 주어진 환경에서 "참(in)"인지 확인한다.
// 정당화의 선행조건이 특정 환경에서 모두 충족되는지 판단하는 데 사용된다.
func SupportingAntecedent(nodes []*TmsNode, env *Env) bool {
	for _, node := range nodes {
		if !InNode(node, env) {
			return false
		}
	}
	return true
}

// RemoveNode는 ATMS에서 노드를 제거한다.
// 다른 정당화의 선행조건으로 사용 중인 노드(Consequences가 비어있지 않은 노드)는 제거할 수 없다.
//
// 제거 과정:
// 1. 전체 노드 목록에서 삭제
// 2. 이 노드의 각 정당화에서, 선행조건 노드들의 역참조를 정리
// 3. 이 노드의 레이블 환경들에서 역참조를 정리
func RemoveNode(node *TmsNode) error {
	if len(node.Consequences) > 0 {
		return fmt.Errorf("can't remove node with consequences")
	}
	atms := node.ATMS
	atms.Nodes = deleteNodeOnce(atms.Nodes, node)
	// 이 노드의 정당화에서 선행조건들의 역참조 정리
	for _, just := range node.Justs {
		for _, ant := range just.Antecedents {
			ant.Consequences = deleteJustOnce(ant.Consequences, just)
		}
	}
	// 이 노드의 레이블 환경에서 역참조 정리
	for _, env := range node.Label {
		env.Nodes = deleteNodeOnce(env.Nodes, node)
	}
	return nil
}

// ============================================================================
// 환경(Environment) 생성 및 확장
// ============================================================================

// CreateEnv는 주어진 가정 목록으로 새 환경을 생성한다.
// 생성된 환경은 환경 테이블에 등록되고, 기존 nogood와의 포섭 관계를 검사한다.
//
// assumptions가 nil이면 빈 환경이 생성된다.
func CreateEnv(atms *ATMS, assumptions []*TmsNode) *Env {
	atms.EnvCounter++
	e := &Env{
		Index:       atms.EnvCounter,
		Assumptions: assumptions,
		Count:       len(assumptions),
	}
	// 환경 테이블에 등록 (가정 개수별로 분류)
	atms.EnvTable = InsertInTable(atms.EnvTable, e)
	// 기존 nogood의 부분집합인지 확인 - 부분집합이면 이 환경도 자동으로 nogood
	SetEnvContradictory(atms, e)
	return e
}

// UnionEnv는 두 환경의 합집합을 반환한다.
// 합집합 과정에서 nogood가 발견되면 nil을 반환한다.
//
// 최적화: 가정 수가 적은 환경을 먼저 처리하여 효율적으로 합집합을 구성한다.
// 각 가정을 하나씩 ConsEnv로 추가하면서, 중간 결과가 nogood가 되면 즉시 중단한다.
func UnionEnv(e1, e2 *Env) *Env {
	// 가정이 적은 쪽을 기준으로 반복 (효율성)
	if e1.Count > e2.Count {
		e1, e2 = e2, e1
	}
	// e1의 각 가정을 e2에 순차적으로 추가
	for _, assume := range e1.Assumptions {
		e2 = ConsEnv(assume, e2)
		if e2.IsNogood() {
			return nil // 중간 결과가 nogood면 합집합 불가
		}
	}
	return e2
}

// ConsEnv는 환경에 가정 하나를 추가한다.
// 동일한 가정 집합을 가진 기존 환경이 있으면 그것을 반환한다 (환경 공유/캐싱).
//
// 이 캐싱은 ATMS의 핵심 최적화이다:
// 같은 가정 집합의 환경을 중복 생성하지 않아 메모리와 비교 비용을 절약한다.
func ConsEnv(assumption *TmsNode, env *Env) *Env {
	// 가정을 정렬된 위치에 삽입하여 새 가정 목록 생성
	nassumes := OrderedInsert(assumption, env.Assumptions, AssumptionOrder)
	// 동일한 가정 집합의 기존 환경 검색
	existing := LookupEnv(nassumes)
	if existing != nil {
		return existing
	}
	// 없으면 새 환경 생성
	return CreateEnv(assumption.ATMS, nassumes)
}

// FindOrMakeEnv는 주어진 가정 목록으로 환경을 찾거나 새로 생성한다.
// 가정이 없으면 빈 환경을 반환한다.
func FindOrMakeEnv(assumptions []*TmsNode, atms *ATMS) *Env {
	if len(assumptions) == 0 {
		return atms.EmptyEnv
	}
	existing := LookupEnv(assumptions)
	if existing != nil {
		return existing
	}
	return CreateEnv(atms, assumptions)
}

// ============================================================================
// 환경 테이블 (Env Table) 관리
// ============================================================================

// InsertInTable은 환경을 테이블에 삽입한다.
// 테이블은 가정 개수(Count)별로 정렬된 버킷 목록이다.
// 같은 Count의 버킷이 있으면 해당 버킷에 추가하고, 없으면 새 버킷을 정렬 위치에 삽입한다.
func InsertInTable(table []EnvTableEntry, env *Env) []EnvTableEntry {
	count := env.Count
	// 같은 크기의 기존 버킷 검색
	for i := range table {
		if table[i].Count == count {
			table[i].Envs = append(table[i].Envs, env)
			return table
		}
	}
	// 새 버킷을 정렬 위치에 삽입
	newEntry := EnvTableEntry{Count: count, Envs: []*Env{env}}
	if len(table) == 0 {
		return []EnvTableEntry{newEntry}
	}
	// 이진 탐색으로 삽입 위치 결정
	idx := sort.Search(len(table), func(i int) bool {
		return table[i].Count >= count
	})
	// idx 위치에 삽입
	table = append(table, EnvTableEntry{})
	copy(table[idx+1:], table[idx:])
	table[idx] = newEntry
	return table
}

// LookupEnv는 주어진 가정 목록과 정확히 일치하는 기존 환경을 검색한다.
// 환경 테이블에서 같은 크기의 버킷을 찾고, 그 안에서 가정이 일치하는 환경을 반환한다.
// 찾지 못하면 nil을 반환한다.
func LookupEnv(assumes []*TmsNode) *Env {
	if len(assumes) == 0 {
		return nil
	}
	atms := assumes[0].ATMS
	count := len(assumes)
	for _, entry := range atms.EnvTable {
		if entry.Count == count {
			for _, env := range entry.Envs {
				if assumptionsEqual(env.Assumptions, assumes) {
					return env
				}
			}
			return nil
		}
	}
	return nil
}

// SubsetEnv는 e1이 e2의 부분집합인지 확인한다.
// e1의 모든 가정이 e2에도 포함되어 있으면 true를 반환한다.
// 같은 환경이면 자명하게 true이다.
func SubsetEnv(e1, e2 *Env) bool {
	if e1 == e2 {
		return true
	}
	// e1의 가정 수가 e2보다 많으면 부분집합일 수 없음
	if e1.Count > e2.Count {
		return false
	}
	return isSubset(e1.Assumptions, e2.Assumptions)
}

// CompareEnv는 두 환경의 포섭 관계를 비교한다.
// 반환값:
//   CompareEQ   - 두 환경이 동일
//   CompareS12  - e1이 e2의 진부분집합 (e1이 더 일반적)
//   CompareS21  - e2가 e1의 진부분집합 (e2가 더 일반적)
//   CompareNone - 부분집합 관계 없음
func CompareEnv(e1, e2 *Env) EnvCompareResult {
	if e1 == e2 {
		return CompareEQ
	}
	if e1.Count < e2.Count {
		if isSubset(e1.Assumptions, e2.Assumptions) {
			return CompareS12 // e1 ⊂ e2
		}
		return CompareNone
	}
	if isSubset(e2.Assumptions, e1.Assumptions) {
		return CompareS21 // e2 ⊂ e1
	}
	return CompareNone
}

// ============================================================================
// Nogood 처리 - 모순 환경 관리
// ============================================================================

// NewNogood는 환경을 nogood(모순)으로 표시하고 그 영향을 전파한다.
//
// 처리 과정:
// 1. 환경을 nogood로 표시
// 2. 이 환경을 레이블에서 가진 모든 노드에서 제거 (RemoveEnvFromLabels)
// 3. nogood 테이블에 추가
// 4. 이 환경의 상위집합인 기존 nogood를 제거 (새 nogood가 더 일반적이므로)
// 5. 환경 테이블에서 이 환경의 상위집합도 nogood로 표시
//
// 예: {A,B}가 nogood가 되면, {A,B,C}, {A,B,D} 등도 모두 nogood가 된다.
func NewNogood(atms *ATMS, cenv *Env, just interface{}) {
	if atms.Debugging {
		debugging(atms, "\n  %s new minimal nogood.", cenv)
	}
	cenv.Nogood = just
	// 이 환경을 사용하는 모든 노드의 레이블에서 제거
	RemoveEnvFromLabels(cenv, atms)
	// nogood 테이블에 등록
	atms.NogoodTable = InsertInTable(atms.NogoodTable, cenv)
	count := cenv.Count
	// 새 nogood의 상위집합인 기존 nogood를 제거 (더 일반적인 것만 유지)
	for i := range atms.NogoodTable {
		if atms.NogoodTable[i].Count > count {
			filtered := make([]*Env, 0, len(atms.NogoodTable[i].Envs))
			for _, old := range atms.NogoodTable[i].Envs {
				if !SubsetEnv(cenv, old) {
					filtered = append(filtered, old)
				}
			}
			atms.NogoodTable[i].Envs = filtered
		}
	}
	// 환경 테이블에서 새 nogood의 상위집합인 환경들도 nogood로 전파
	for i := range atms.EnvTable {
		if atms.EnvTable[i].Count > count {
			for _, old := range atms.EnvTable[i].Envs {
				if !old.IsNogood() && SubsetEnv(cenv, old) {
					old.Nogood = cenv
					RemoveEnvFromLabels(old, atms)
				}
			}
		}
	}
}

// SetEnvContradictory는 새로 생성된 환경이 기존 nogood의 상위집합인지 확인한다.
// 기존 nogood가 이 환경의 부분집합이면, 이 환경도 자동으로 nogood가 된다.
//
// nogood 테이블은 크기 순으로 정렬되어 있으므로,
// 현재 환경보다 큰 nogood를 만나면 탐색을 조기 종료한다.
func SetEnvContradictory(atms *ATMS, env *Env) bool {
	if env.IsNogood() {
		return true
	}
	count := env.Count
	for _, entry := range atms.NogoodTable {
		if entry.Count > count {
			return false // 이보다 큰 nogood는 부분집합일 수 없음
		}
		for _, cenv := range entry.Envs {
			if SubsetEnv(cenv, env) {
				env.Nogood = cenv // 이 nogood가 env의 부분집합 → env도 nogood
				return true
			}
		}
	}
	return false
}

// RemoveEnvFromLabels는 모든 노드의 레이블에서 주어진 환경을 제거한다.
// 환경이 nogood가 되었을 때 호출된다.
//
// 또한 이 환경에 등록된 규칙들을 큐에 넣어 재평가하도록 한다.
func RemoveEnvFromLabels(env *Env, atms *ATMS) {
	// 이 환경에 등록된 규칙을 실행 큐에 추가
	if atms.EnqueueProcedure != nil {
		for _, rule := range env.Rules {
			atms.EnqueueProcedure(rule)
		}
		env.Rules = nil
	}
	// 이 환경을 레이블에 가진 모든 노드에서 제거
	for _, node := range env.Nodes {
		node.Label = deleteEnvOnce(node.Label, env)
	}
}

// ============================================================================
// 해석(Interpretation) 구성
// ============================================================================

// Interpretations는 주어진 선택 집합(choice sets)에서 일관된 해석을 구성한다.
// 각 선택 집합에서 하나씩 선택하여 조합한 환경이 nogood가 아닌 것들을 반환한다.
//
// 이 함수는 진단 추론, 제약 만족 등의 문제에서 가능한 해를 열거하는 데 사용된다.
//
// choiceSets: 각 집합에서 하나를 선택해야 하는 노드 그룹들
// defaults: 가능하면 포함하려는 기본 가정들 (최대한 포함)
//
// 깊이 우선 탐색으로 해를 구성하며, 포섭되는 해는 제거한다.
func Interpretations(atms *ATMS, choiceSets [][]*TmsNode, defaults []*TmsNode) []*Env {
	if atms.Debugging {
		fmt.Fprintf(os.Stderr, "\n Constructing interpretations depth-first...")
	}

	var solutions []*Env

	// 선택 집합 확장: 각 노드를 그 노드의 레이블 환경들로 대체
	expandedChoiceSets := make([][]*Env, len(choiceSets))
	for i, altSet := range choiceSets {
		var envs []*Env
		for _, alt := range altSet {
			envs = append(envs, alt.Label...)
		}
		expandedChoiceSets[i] = envs
	}

	// 깊이 우선 탐색으로 해를 구성
	if len(expandedChoiceSets) > 0 {
		for _, choice := range expandedChoiceSets[0] {
			getDepthSolutions1(choice, expandedChoiceSets[1:], &solutions)
		}
	}

	solutions = deleteNilEnvs(solutions)

	if len(solutions) == 0 {
		if len(choiceSets) > 0 {
			return nil // 가능한 해가 없음
		}
		solutions = []*Env{atms.EmptyEnv} // 선택 집합이 없으면 빈 환경이 해
	}

	// 기본 가정으로 해를 확장 (최대한 많은 기본 가정 포함)
	if len(defaults) > 0 {
		oldSolutions := solutions
		solutions = nil
		for _, solution := range oldSolutions {
			extendViaDefaults(solution, defaults, defaults, &solutions)
		}
	}

	return deleteNilEnvs(solutions)
}

// getDepthSolutions1은 해석 구성을 위한 깊이 우선 탐색의 재귀 함수이다.
// 현재까지 구성된 환경(solution)에 남은 선택 집합(choiceSets)의 선택을 추가하며 탐색한다.
//
// 기저 사례: 선택 집합이 비면, 현재 해를 기존 해들과 포섭 비교 후 추가
// 재귀 사례: 현재 선택 집합의 각 환경과 합집합을 구성하여 재귀 호출
func getDepthSolutions1(solution *Env, choiceSets [][]*Env, solutions *[]*Env) {
	if len(choiceSets) == 0 {
		// 기존 해와 포섭 관계 확인
		subsumed := false
		for i, old := range *solutions {
			if old == nil {
				continue
			}
			cmp := CompareEnv(old, solution)
			switch cmp {
			case CompareEQ, CompareS12:
				subsumed = true // 기존 해가 더 일반적이거나 동일 → 새 해 불필요
			case CompareS21:
				(*solutions)[i] = nil // 새 해가 더 일반적 → 기존 해 제거
			}
			if subsumed {
				break
			}
		}
		if !subsumed {
			*solutions = append(*solutions, solution)
		}
		return
	}

	// nogood 환경은 확장하지 않음
	if solution.IsNogood() {
		return
	}

	// 다음 선택 집합의 각 환경과 합집합을 시도
	for _, choice := range choiceSets[0] {
		newSolution := UnionEnv(solution, choice)
		if newSolution != nil && !newSolution.IsNogood() {
			getDepthSolutions1(newSolution, choiceSets[1:], solutions)
		}
	}
}

// extendViaDefaults는 해석을 기본 가정들로 확장한다.
// 각 기본 가정을 추가할 수 있으면 추가하고, nogood가 되면 건너뛴다.
//
// 최종적으로 "극대(maximal)" 해만 반환한다:
// 더 이상 기본 가정을 추가해도 nogood가 되지 않는 기본 가정이 없는 경우에만 해로 인정.
func extendViaDefaults(solution *Env, remaining []*TmsNode, original []*TmsNode, solutions *[]*Env) {
	if len(remaining) == 0 {
		// 중복 해 확인
		for _, s := range *solutions {
			if s == solution {
				return
			}
		}
		// 극대성 확인: 아직 추가 가능한 기본 가정이 있으면 이 해는 극대가 아님
		for _, def := range original {
			inAssumptions := false
			for _, a := range solution.Assumptions {
				if a == def {
					inAssumptions = true
					break
				}
			}
			if inAssumptions {
				continue
			}
			ce := ConsEnv(def, solution)
			if ce.IsNogood() {
				continue
			}
			// 추가 가능한 기본 가정이 있음 → 극대가 아니므로 해로 인정하지 않음
			return
		}
		*solutions = append(*solutions, solution)
		return
	}

	// 현재 기본 가정을 추가해보기
	newSolution := ConsEnv(remaining[0], solution)
	if !newSolution.IsNogood() {
		extendViaDefaults(newSolution, remaining[1:], original, solutions)
	}
	// 현재 기본 가정 없이도 시도 (다른 기본 가정들로 확장)
	extendViaDefaults(solution, remaining[1:], original, solutions)
}

// ============================================================================
// 설명(Explanation) 생성
// ============================================================================

// ExplainNode는 특정 환경에서 노드가 왜 참인지에 대한 설명을 생성한다.
// 설명은 정당화(Just)와 가정(assumeExplanation)의 목록으로 구성된다.
//
// 반환되는 설명은 노드가 참이 되기까지의 추론 체인을 보여준다:
// - 가정인 노드는 assumeExplanation으로 표시
// - 추론된 노드는 해당 정당화(Just)와 선행조건들의 재귀적 설명으로 구성
func ExplainNode(node *TmsNode, env *Env) []interface{} {
	return explainNode1(env, node, nil, nil)
}

// assumeExplanation은 설명에서 가정을 나타내는 구조체이다.
// 해당 노드가 가정으로서 참으로 받아들여졌음을 표시한다.
type assumeExplanation struct {
	Node *TmsNode
}

// explainNode1은 ExplainNode의 재귀 헬퍼 함수이다.
// 순환 참조를 방지하기 위해 queuedNodes로 이미 탐색 중인 노드를 추적한다.
//
// 탐색 순서:
// 1. 이미 탐색 중인 노드면 nil 반환 (순환 방지)
// 2. 가정 노드이고 해당 환경에 포함되면 가정 설명 추가
// 3. 이미 설명된 노드면 기존 설명 반환
// 4. 각 정당화를 시도하여 모든 선행조건이 env에서 성립하는 것을 찾아 설명 구성
func explainNode1(env *Env, node *TmsNode, queuedNodes []*TmsNode, explanation []interface{}) []interface{} {
	// 순환 탐색 방지: 이미 탐색 중인 노드면 실패
	for _, q := range queuedNodes {
		if q == node {
			return nil
		}
	}

	// 가정 노드이고 이 환경에 포함되어 있으면, 가정 설명으로 추가
	if node.IsAssumption {
		for _, a := range env.Assumptions {
			if a == node {
				return append([]interface{}{&assumeExplanation{Node: node}}, explanation...)
			}
		}
	}

	// 이미 설명된 노드인지 확인 (중복 설명 방지)
	for _, entry := range explanation {
		switch e := entry.(type) {
		case *assumeExplanation:
			if e.Node == node {
				return explanation
			}
		case *Just:
			if e.Consequence == node {
				return explanation
			}
		}
	}

	// 현재 노드를 탐색 중 목록에 추가
	queuedNodes = append([]*TmsNode{node}, queuedNodes...)

	// 각 정당화를 시도: 모든 선행조건이 env에서 성립하는 정당화를 찾음
	for _, just := range node.Justs {
		// 선행조건이 모두 이 환경에서 참인지 확인
		allIn := true
		for _, a := range just.Antecedents {
			if !InNode(a, env) {
				allIn = false
				break
			}
		}
		if !allIn {
			continue
		}

		// 선행조건들의 설명을 재귀적으로 수집
		newExplanation := explanation
		success := true
		for _, a := range just.Antecedents {
			newExplanation = explainNode1(env, a, queuedNodes, newExplanation)
			if newExplanation == nil {
				success = false
				break
			}
		}
		if success {
			// 이 정당화를 설명에 추가하고 반환
			return append([]interface{}{just}, newExplanation...)
		}
	}
	return nil // 설명을 구성할 수 없음
}

// ============================================================================
// 출력 (Printing) 함수들
// ============================================================================

// WhyNode는 노드의 정보와 레이블(참이 되는 환경들)을 출력한다.
// 형식: <prefix datum, {env1 env2 ...}>
func WhyNode(node *TmsNode, w io.Writer, prefix string) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n<%s%v,{", prefix, node.Datum)
	for _, e := range node.Label {
		EnvString(e, w)
	}
	fmt.Fprintf(w, "}>")
}

// WhyNodes는 ATMS의 모든 노드를 출력한다.
// 노드 목록을 역순으로 출력하여 생성 순서대로 보여준다.
func WhyNodes(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	// 역순 출력 (Lisp 원본의 nreverse와 동일한 효과)
	for i := len(atms.Nodes) - 1; i >= 0; i-- {
		WhyNode(atms.Nodes[i], w, "")
	}
}

// NodeJustifications는 노드에 대한 모든 정당화를 출력한다.
// 각 정당화의 출처(informant)와 선행조건들을 보여준다.
func NodeJustifications(node *TmsNode, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n For %s:", NodeString(node))
	for _, j := range node.Justs {
		PrintJustification(j, w)
	}
}

// PrintJustification은 단일 정당화의 상세 정보를 출력한다.
// 출처(informant)와 각 선행조건 노드의 정보를 보여준다.
func PrintJustification(j *Just, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n  %v, ", j.Informant)
	for _, a := range j.Antecedents {
		WhyNode(a, w, "     ")
	}
}

// E는 ATMS에서 인덱스 번호로 환경을 검색한다.
// 환경 테이블의 모든 버킷을 순회하여 일치하는 환경을 반환한다.
func E(atms *ATMS, n int) *Env {
	for _, bucket := range atms.EnvTable {
		for _, env := range bucket.Envs {
			if env.Index == n {
				return env
			}
		}
	}
	return nil
}

// PrintEnv는 환경의 정보를 출력한다.
// nogood 환경은 "*" 표시와 함께 출력된다.
func PrintEnv(e *Env, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	nogoodMark := " "
	if e.IsNogood() {
		nogoodMark = "* " // nogood 환경 표시
	}
	fmt.Fprintf(w, "\n%s:%s", e, nogoodMark)
	EnvString(e, w)
}

// EnvString은 환경의 가정 목록을 문자열로 출력한다.
// 형식: {가정1, 가정2, ...}
// 가정들은 이름순으로 정렬하여 출력한다.
func EnvString(e *Env, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	assumptions := e.Assumptions
	var strs []string
	var printer func(*TmsNode) string
	if len(assumptions) > 0 {
		printer = assumptions[0].ATMS.NodeString
	}
	for _, a := range assumptions {
		strs = append(strs, printer(a))
	}
	sort.Strings(strs) // 알파벳 순으로 정렬하여 일관된 출력
	fmt.Fprintf(w, "{%s}", strings.Join(strs, ","))
}

// ============================================================================
// 전역 데이터 출력
// ============================================================================

// PrintNogoods는 ATMS의 nogood 테이블을 출력한다.
// 모든 모순 환경과 그 가정들을 보여준다.
func PrintNogoods(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	PrintEnvTable(atms.NogoodTable, w)
}

// PrintEnvs는 ATMS의 환경 테이블을 출력한다.
// 시스템에 존재하는 모든 환경을 보여준다.
func PrintEnvs(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	PrintEnvTable(atms.EnvTable, w)
}

// PrintEnvTable은 환경 테이블의 모든 환경을 출력한다.
// 버킷별로 순회하며 각 환경을 PrintEnv로 출력한다.
func PrintEnvTable(table []EnvTableEntry, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	for _, bucket := range table {
		for _, env := range bucket.Envs {
			PrintEnv(env, w)
		}
	}
}

// PrintATMSStatistics는 ATMS의 환경 테이블과 nogood 테이블에 대한 통계를 출력한다.
// 각 버킷의 크기와 환경 수를 보여준다.
func PrintATMSStatistics(atms *ATMS) {
	PrintTable("\n For env table:", atms.EnvTable)
	PrintTable("\n For nogood table:", atms.NogoodTable)
}

// PrintTable은 테이블의 요약 통계를 출력한다.
// 각 버킷의 가정 개수와 해당 크기의 환경 수를 보여준다.
func PrintTable(msg string, table []EnvTableEntry) {
	fmt.Print(msg)
	for _, entry := range table {
		fmt.Printf("\n   Length %d, %d", entry.Count, len(entry.Envs))
	}
}

// ============================================================================
// 내부 헬퍼 함수들
// ============================================================================

// deleteNilEnvs는 환경 슬라이스에서 nil 항목을 제거한다.
// Update, Weave 등에서 환경이 무효화(nil)된 후 정리하는 데 사용된다.
func deleteNilEnvs(envs []*Env) []*Env {
	result := make([]*Env, 0, len(envs))
	for _, e := range envs {
		if e != nil {
			result = append(result, e)
		}
	}
	return result
}

// deleteNilEnvsFromSlice는 deleteNilEnvs와 동일한 기능이다.
// Lisp 원본의 (delete nil envs :test #'eq) 호출에 대응한다.
func deleteNilEnvsFromSlice(envs []*Env) []*Env {
	return deleteNilEnvs(envs)
}

// deleteNodeOnce는 노드 슬라이스에서 주어진 노드의 첫 번째 출현을 제거한다.
// 포인터 동일성으로 비교한다.
func deleteNodeOnce(nodes []*TmsNode, node *TmsNode) []*TmsNode {
	for i, n := range nodes {
		if n == node {
			return append(nodes[:i], nodes[i+1:]...)
		}
	}
	return nodes
}

// deleteJustOnce는 정당화 슬라이스에서 주어진 정당화의 첫 번째 출현을 제거한다.
// 포인터 동일성으로 비교한다.
func deleteJustOnce(justs []*Just, just *Just) []*Just {
	for i, j := range justs {
		if j == just {
			return append(justs[:i], justs[i+1:]...)
		}
	}
	return justs
}

// deleteEnvOnce는 환경 슬라이스에서 주어진 환경의 첫 번째 출현을 제거한다.
// 포인터 동일성으로 비교한다.
func deleteEnvOnce(envs []*Env, env *Env) []*Env {
	for i, e := range envs {
		if e == env {
			return append(envs[:i], envs[i+1:]...)
		}
	}
	return envs
}

// copyEnvSlice는 환경 슬라이스의 얕은 복사본을 반환한다.
// 원본 슬라이스를 보호하면서 수정이 필요한 경우에 사용한다.
func copyEnvSlice(envs []*Env) []*Env {
	if envs == nil {
		return nil
	}
	result := make([]*Env, len(envs))
	copy(result, envs)
	return result
}

// assumptionsEqual은 두 가정 슬라이스가 동일한지 확인한다.
// 같은 순서의 같은 노드들(포인터 동일성)을 가지면 true를 반환한다.
// 환경 검색(LookupEnv)에서 기존 환경과의 일치 여부를 판단하는 데 사용된다.
func assumptionsEqual(a, b []*TmsNode) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// isSubset은 sub의 모든 원소가 super에 포함되는지 확인한다.
// 두 슬라이스 모두 인덱스 순으로 정렬되어 있다고 가정한다.
// 정렬된 배열의 병합 검색(merge scan) 알고리즘을 사용하여 O(n+m) 시간에 수행한다.
func isSubset(sub, super []*TmsNode) bool {
	j := 0
	for _, s := range sub {
		// super에서 s의 인덱스 이상인 원소를 찾을 때까지 전진
		for j < len(super) && super[j].Index < s.Index {
			j++
		}
		// 일치하는 원소가 없으면 부분집합이 아님
		if j >= len(super) || super[j] != s {
			return false
		}
		j++
	}
	return true
}
