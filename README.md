# 문화 관공 데이터 분석 대회(2022)
<br>
<br>

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/20cbb375-4da1-4433-aca1-72d240572357" align = "center" width = "70%">
<br>

#### *팀원과 함께한 첫 공모전, [통계데이터 분석 활용대회](https://github.com/JInseed/Statistics_Data_Analysis_Competition.git) 와 동시에 참가하며 둘 다 제대로 완성하지 못했으며 과정도 미흡*
<br>

## 진행기간
> **2022/06/15~07/21**


<br>

## 팀소개
|    소속    |          전공           |  이름  |
| :--------: | :---------------------: | :----: |
| 동국대학교 | 통계학과/데이터사이언스 | 정정룡 |
| 동국대학교 | 통계학과/데이터사이언스 | 김평진 |
| 동국대학교 | 통계학과/데이터사이언스| 황영우 |
<br>

## 사용 데이터
> **한국관광공사 : TourAPI3.0 시군구별 업종별 관광데이터(시설물)**

> **문화체육관광부 : 국민여행조사**

> **신한카드 : 카드 매출 데이터**
<br>

## 개발 환경
> **R, Python**
<br>

### 역할
> **자료조사 및 기획,  전처리, 시각화, 모델링, 보고서 작성**
<br>

## 분석 주제
> **테마 별로 보는 우리나라 관광지 여행 활성화**
<br>

## 분석 목적
> **코로나 사회적  거리두기가 해제됨으로써 향후 여행이 활성화 될 것으로 예상. 이에 연도별로 국민여행조사 데이터를 살펴본 결과 특정 지역에 몰리는 현상이 심화되었음을 확인. 따라서 향후 여행지 선택에 있어 다양한 지역의 활성화를 장려하고자 함**
<br>

## 분석 요약
1. 국민여행조사 데이터 EDA
2. Tour 3.0 API
    1. 지역별 테마별 시설물 데이터 수집
    2. PCA
    3. PCA 데이터를 활용하여 지역을 군집화(kmeans) ⇒ 여행 테마 생성
    4. 주성분 해석 및 군집 해석
3. 신한카드 데이터
    1. 여행 매출이라 간주할 수 있는 변수 추출
    2. 외식, 숙박의 매출변수를 이용하여 지역 군집화(kmeans) ⇒ 활성화 정도 생성
4. 2, 3 에서 진행한 군집화 결과를 결합하여 여행 활성화 변수 생성
5. 회귀 모델 활용하여 여행 만족도에 영향을 주는 변수 탐색(제대로 진행하지 못함)
6. 여행 테마별 지역 활성화 방안 제시(진행하지 못함)
<br>

## 분석 과정

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/97fb63f0-aa10-49a8-b370-837a987dbd0a" align = "center" width = "80%">
<br><br>

<p align = "center" width = "100%">
  <img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/31d7ccac-3543-4abe-897c-c203d4ed1e90" width = "49%">
  <img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/1f481f25-5b7a-4637-8364-f25a193095cc" width = "49%">
</p>
<br>

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/3e5fe0dd-5299-4608-992a-9ba289e501ba" align = "center" width = "80%">
<br><br>

<p align = "center" width = "100%">
  <img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/91282ac8-d5e4-415e-9657-75e14974f7ad" width = "49%">
  <img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/c5b16cc3-7837-4431-8803-97fd5992cc6c" width = "49%">
</p>
<br>

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/97b6c2f8-bf06-4c43-8332-96cd0df6d87b" align = "center" width = "80%">
<br><br>

- 기존 변수로 했을 때 `silhouette = 0.301`
- 주성분 `누적 설명력 80% 이상`으로 3개로 타당
    - pc1 : 36.2%
    - pc2 : 29.7%
    - pc3 : 19.9%
<br>

### *주성분 및 군집 해석*

#### PC1: 문화 쇼핑 휴양이 높으며 자연레포츠역사가 낮은 지역
#### PC2: 역사가 매우 높으며 자연은 낮다
#### PC3: 자연이 매우 높으며 레포츠는 매우 낮다
<br>

### *군집 해석*

#### 0번: 문화 쇼핑 휴양이 높은 지역
#### 1번: 자연이 매우 높은 지역
#### 2번: 역사가 매우 높은 지역
#### 3번: PC3가 매우 낮은, 즉 레포츠 발달지역
<br>

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/8d4c01ad-77e1-49b4-97f5-0a952ea70e36" align = "center" width = "80%">
<br><br>

<img src = "https://github.com/JInseed/Culture_Data_Analysis_Competition/assets/120428959/d4aa25cb-0aef-4f17-991b-6d5e99aec50d" align = "center" width = "80%">
<br><br>

## 결과 해석

- `PC1`은 문화체험, 쇼핑, 휴양에서 높은 양의 점수를 가지며 자연과 레포츠, 역사에서는 음의 점수를 가지므로 도심에서 즐길 수 있는 `도심_힐링지수`라고 명명
- `PC2`는 역사에서 매우 높은 양의 점수를 가지므로 `역사지수`라고 명명
- `PC3`는 자연에서 높은 양의 점수와 레포츠에서 낮은 음의 점수를 가지며 `순수자연 지수`라고 명명
- 위 세 가지 주성분으로 군집 별 해석 진행
    - `첫 번째 군집`은 `PC1`이 매우 높은 `문화쇼핑휴양군집`으로 해석
    - `두 번째 군집`은 `PC3`가 매우 높은 `자연군집`으로 해석
    - `세 번째 군집`은 `PC2`가 매우 높은 `역사군집`으로 해석
    - `네 번째 군집`은 `PC3`가 매우 낮은 `레포츠군집`으로 해석
<br>

- 군집 내에서도 여행 활성화 정도를 파악하기 위해 신한카드 데이터 내 시도별 외식매출과 숙박매출액 데이터를 사용하여 군집화를 진행
- `카드 매출`은 실제 해당 지역에서 어떤 식으로 소비가 이루어지는지를 알 수 있는 지표로 투어 데이터 내 숙박업소와 음식점의 수로 발달 정도를 파악하는 것보다 더 `현실적이며 유의미한 지표`라고 판단
- 매출변수들 또한 TourAPI 3.0 데이터와 같이 `지역별 편차`가 크며, 분포를 확인하였을 때 오른쪽으로 기울어져 있어 매우 높은 값은 군집화에 영향을 줄 수 있다고 판단하여 `로그변환`을 통해 편차와 이상치를 모두 해소
- 군집 개수 별 실루엣 그래프에서 3개 일 때 0.392로 2개인 군집보다 낮았지만, elbow plot은 3개가 최적임을 보여주었으며 군집 내 계수의 편차가 균일했기 때문에 최종적으로 `3개의 군집`으로 진행
- 각각의 군집은 숙박, 음식의 활성화 정도로 상 중 하로 군집. 앞서 진행한 테마 군집화와 발달 군집화 두 가지를 결합하여 `테마 별 상, 중, 하로 총 12개의 군집`을 형성

#### *※ 회귀모델을 통해 여행만족도에 주는 변수를 찾으려 했으나, 계절, 여행 활동, 여행지선택이유를 제외하고 알맞은 변수가 설문에 존재하지 않아서 해석에 있어 아쉬움이 있었음. 또한 테마 별로 모델을 돌리고 싶었으나 시간 상의 문제로 진행하지 못했고 더 자세한 해석과 해결방안을 제시하지 못함*
<br>

## 시사점 및 보완할 점

- 첫 공모전이다 보니 너무 세세한 것들까지 완벽하게 하려 해서 군집화에서 특히 시간을 너무 소비
- 코드 정리와 진행 과정이 미숙
- 회귀 모델을 xgboost를 제외하고 돌리지 않았고 자세한 해석도 진행하지 못함
- 해석과 더불어 여행 활성화 해결방안을 제시하지 못해 매우 아쉬움
