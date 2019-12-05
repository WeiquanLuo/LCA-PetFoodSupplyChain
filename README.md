
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Interesting question

The goal of this project is to study the environmental impact of a
certain amount of production with Economic Input-Output Life Cycle
Assessment (EIO-LCA) method, which estimates activities in our economy
in the materials and energy resources required for and the environmental
impact resulting from. The environmental impacts involove conventional
air poluten (CAP), greenhouse gass (GHG), and toix release (TOX).
Cradle-to-grave is the full Life Cycle Assessment from resource
extraction to use phase and disposal phase. Specificallym, this analysis
is base on the Cradle-to-grave EIO-LCA result to further understand how
all industrial stages of producing Million Dollars product *in Dog and
Cat Food Manufacturing* (code 311111 in NAICS 2002) are different in
environmental impact. The study aim to answer the following questions:

  - which industry(s) have larger impact among all industries?
  - what are the relationship between some impact relative to the input
    (ie. Energy, water withdraw)?
  - how the outlier industry(s) behave in linear regression models

## Hightlight

1.  for dog and cat food Manufacturing, the greatest environmental
    impacts come frome any raw material production industries such as
    agricultural farming.

2.  we successfully fit linear regression model on the following
    environmental impact using energy and water withdraw as input:

<!-- end list -->

  - Emissions of Nitrogen Oxides to Air,
  - Emissions of Sulfur Dioxide to Air,
  - weighting of greenhouse gas emissions into the air from the
    production,
  - Emissions of Carbon Dioxide (CO2) into the air from fossil fuel
    combus.

<!-- end list -->

3.  most of industries use either NonFossoil Eletrecity or Fossoil
    Eletrecity
4.  for those industries using biowaste as energy source have higher
    impact in toxic.

## Workflow

<center>

![LCA:Pet Food Supply
Chain](doc/LCA_%20Pet%20Food%20Supply%20Chain%20.png)

</center>

# Data description

The dataset for this project is the first pass life cycle assessment
results for cat and dog food manufacturing. It provides the
environmental impact information into the pet food supply chain. The LCA
data was generated through EIO-LCA
website(<http://www.eiolca.net/cgi-bin/dft/use.pl>). The model for
getting the LCA data is US 2002 producer price benchmark. In order to
get the dog and cat food manufacturing LCA data, the user needs to
select “Food beverage and tobacco” sector group, dog and cat food
manufacturing sector and then the amount of economic activity for this
sector(e.g. 1 millon dollars). After setting up the sector and economic
parameters, The user could select different economical and environmental
impact to get the LCA results. The LCA results are ready for downloading
as excel files. The raw data was lacking the columns name for different
impact feature and index for identifying the different sectors and
sector group. The web scraping is necessary for the columns name and
NAICS sector code.

# Explore the data

After webscaping, combinding the raw data, and manually making minor
modification, we result a datafarme stored as ’dat\_311111\_1M\_v2.csv.

``` r
# data input
dat <- read.csv("data/dat_311111_1M_v2.csv")
# Input columns 
X <- dat %>% 
  select(Coal.TJ, NatGase.TJ, Petrol.TJ, Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal)
psych::describe(X) %>% knitr::kable()
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

vars

</th>

<th style="text-align:right;">

n

</th>

<th style="text-align:right;">

mean

</th>

<th style="text-align:right;">

sd

</th>

<th style="text-align:right;">

median

</th>

<th style="text-align:right;">

trimmed

</th>

<th style="text-align:right;">

mad

</th>

<th style="text-align:right;">

min

</th>

<th style="text-align:right;">

max

</th>

<th style="text-align:right;">

range

</th>

<th style="text-align:right;">

skew

</th>

<th style="text-align:right;">

kurtosis

</th>

<th style="text-align:right;">

se

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Coal.TJ

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0076337

</td>

<td style="text-align:right;">

0.1168583

</td>

<td style="text-align:right;">

0.0000010

</td>

<td style="text-align:right;">

0.0000344

</td>

<td style="text-align:right;">

0.0000015

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

2.321467e+00

</td>

<td style="text-align:right;">

2.321467e+00

</td>

<td style="text-align:right;">

19.34331

</td>

<td style="text-align:right;">

379.4182

</td>

<td style="text-align:right;">

0.0058284

</td>

</tr>

<tr>

<td style="text-align:left;">

NatGase.TJ

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0110593

</td>

<td style="text-align:right;">

0.0797249

</td>

<td style="text-align:right;">

0.0001375

</td>

<td style="text-align:right;">

0.0006286

</td>

<td style="text-align:right;">

0.0002039

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.366673e+00

</td>

<td style="text-align:right;">

1.366673e+00

</td>

<td style="text-align:right;">

13.87669

</td>

<td style="text-align:right;">

217.6585

</td>

<td style="text-align:right;">

0.0039763

</td>

</tr>

<tr>

<td style="text-align:left;">

Petrol.TJ

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0086899

</td>

<td style="text-align:right;">

0.0654656

</td>

<td style="text-align:right;">

0.0000235

</td>

<td style="text-align:right;">

0.0003121

</td>

<td style="text-align:right;">

0.0000348

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.088900e+00

</td>

<td style="text-align:right;">

1.088900e+00

</td>

<td style="text-align:right;">

13.11028

</td>

<td style="text-align:right;">

195.8157

</td>

<td style="text-align:right;">

0.0032651

</td>

</tr>

<tr>

<td style="text-align:left;">

Bio.Waste.TJ

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0019597

</td>

<td style="text-align:right;">

0.0143116

</td>

<td style="text-align:right;">

0.0000010

</td>

<td style="text-align:right;">

0.0000250

</td>

<td style="text-align:right;">

0.0000015

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.806160e-01

</td>

<td style="text-align:right;">

1.806160e-01

</td>

<td style="text-align:right;">

10.26709

</td>

<td style="text-align:right;">

114.4378

</td>

<td style="text-align:right;">

0.0007138

</td>

</tr>

<tr>

<td style="text-align:left;">

NonFossElec.TJ

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0042177

</td>

<td style="text-align:right;">

0.0251197

</td>

<td style="text-align:right;">

0.0001570

</td>

<td style="text-align:right;">

0.0005038

</td>

<td style="text-align:right;">

0.0002298

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

4.231030e-01

</td>

<td style="text-align:right;">

4.231030e-01

</td>

<td style="text-align:right;">

13.11334

</td>

<td style="text-align:right;">

200.3802

</td>

<td style="text-align:right;">

0.0012529

</td>

</tr>

<tr>

<td style="text-align:left;">

Water.Withdrawals.Kgal

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

446.1435342

</td>

<td style="text-align:right;">

7894.3608174

</td>

<td style="text-align:right;">

0.0428940

</td>

<td style="text-align:right;">

0.5317598

</td>

<td style="text-align:right;">

0.0630550

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.580305e+05

</td>

<td style="text-align:right;">

1.580305e+05

</td>

<td style="text-align:right;">

19.78841

</td>

<td style="text-align:right;">

391.9673

</td>

<td style="text-align:right;">

393.7349309

</td>

</tr>

</tbody>

</table>

``` r
M <- cor(X)
corrplot::corrplot(M, method="color")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
# target columns 
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
ys <- cbind(CPA, GHG, TOX)
psych::describe(ys) %>% knitr::kable()
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

vars

</th>

<th style="text-align:right;">

n

</th>

<th style="text-align:right;">

mean

</th>

<th style="text-align:right;">

sd

</th>

<th style="text-align:right;">

median

</th>

<th style="text-align:right;">

trimmed

</th>

<th style="text-align:right;">

mad

</th>

<th style="text-align:right;">

min

</th>

<th style="text-align:right;">

max

</th>

<th style="text-align:right;">

range

</th>

<th style="text-align:right;">

skew

</th>

<th style="text-align:right;">

kurtosis

</th>

<th style="text-align:right;">

se

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CO.t

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0222147

</td>

<td style="text-align:right;">

0.2952078

</td>

<td style="text-align:right;">

0.0000385

</td>

<td style="text-align:right;">

0.0006645

</td>

<td style="text-align:right;">

0.0000571

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

5.888105

</td>

<td style="text-align:right;">

5.888105

</td>

<td style="text-align:right;">

19.523839

</td>

<td style="text-align:right;">

384.79885

</td>

<td style="text-align:right;">

0.0147236

</td>

</tr>

<tr>

<td style="text-align:left;">

NH3.t

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0098300

</td>

<td style="text-align:right;">

0.1389211

</td>

<td style="text-align:right;">

0.0000010

</td>

<td style="text-align:right;">

0.0000086

</td>

<td style="text-align:right;">

0.0000015

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

2.687352

</td>

<td style="text-align:right;">

2.687352

</td>

<td style="text-align:right;">

18.084851

</td>

<td style="text-align:right;">

341.44049

</td>

<td style="text-align:right;">

0.0069288

</td>

</tr>

<tr>

<td style="text-align:left;">

NOx.t

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0080359

</td>

<td style="text-align:right;">

0.0540448

</td>

<td style="text-align:right;">

0.0000280

</td>

<td style="text-align:right;">

0.0003363

</td>

<td style="text-align:right;">

0.0000415

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.707607

</td>

<td style="text-align:right;">

0.707607

</td>

<td style="text-align:right;">

9.977676

</td>

<td style="text-align:right;">

106.60356

</td>

<td style="text-align:right;">

0.0026955

</td>

</tr>

<tr>

<td style="text-align:left;">

PM10.t

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0177165

</td>

<td style="text-align:right;">

0.2850504

</td>

<td style="text-align:right;">

0.0000055

</td>

<td style="text-align:right;">

0.0001408

</td>

<td style="text-align:right;">

0.0000082

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

5.695606

</td>

<td style="text-align:right;">

5.695606

</td>

<td style="text-align:right;">

19.666983

</td>

<td style="text-align:right;">

388.60310

</td>

<td style="text-align:right;">

0.0142170

</td>

</tr>

<tr>

<td style="text-align:left;">

PM2.5.t

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0047236

</td>

<td style="text-align:right;">

0.0694547

</td>

<td style="text-align:right;">

0.0000040

</td>

<td style="text-align:right;">

0.0000720

</td>

<td style="text-align:right;">

0.0000059

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.382822

</td>

<td style="text-align:right;">

1.382822

</td>

<td style="text-align:right;">

19.453251

</td>

<td style="text-align:right;">

382.59517

</td>

<td style="text-align:right;">

0.0034641

</td>

</tr>

<tr>

<td style="text-align:left;">

SO2.t

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0057112

</td>

<td style="text-align:right;">

0.0559066

</td>

<td style="text-align:right;">

0.0000140

</td>

<td style="text-align:right;">

0.0002277

</td>

<td style="text-align:right;">

0.0000208

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1.082028

</td>

<td style="text-align:right;">

1.082028

</td>

<td style="text-align:right;">

17.908593

</td>

<td style="text-align:right;">

339.19226

</td>

<td style="text-align:right;">

0.0027884

</td>

</tr>

<tr>

<td style="text-align:left;">

VOC.t

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0047379

</td>

<td style="text-align:right;">

0.0406288

</td>

<td style="text-align:right;">

0.0000225

</td>

<td style="text-align:right;">

0.0002173

</td>

<td style="text-align:right;">

0.0000334

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.723802

</td>

<td style="text-align:right;">

0.723802

</td>

<td style="text-align:right;">

15.184308

</td>

<td style="text-align:right;">

251.89230

</td>

<td style="text-align:right;">

0.0020264

</td>

</tr>

<tr>

<td style="text-align:left;">

Total.t.CO2e

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

3.8162115

</td>

<td style="text-align:right;">

30.7723669

</td>

<td style="text-align:right;">

0.0171445

</td>

<td style="text-align:right;">

0.1191106

</td>

<td style="text-align:right;">

0.0251975

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

512.173950

</td>

<td style="text-align:right;">

512.173950

</td>

<td style="text-align:right;">

13.299869

</td>

<td style="text-align:right;">

198.03556

</td>

<td style="text-align:right;">

1.5347862

</td>

</tr>

<tr>

<td style="text-align:left;">

CO2.Fossil.t.CO2e

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

1.8345859

</td>

<td style="text-align:right;">

14.4120956

</td>

<td style="text-align:right;">

0.0169625

</td>

<td style="text-align:right;">

0.0901948

</td>

<td style="text-align:right;">

0.0249277

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

257.559803

</td>

<td style="text-align:right;">

257.559803

</td>

<td style="text-align:right;">

14.832228

</td>

<td style="text-align:right;">

248.47182

</td>

<td style="text-align:right;">

0.7188100

</td>

</tr>

<tr>

<td style="text-align:left;">

CO2.Process.t.CO2e

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.1249104

</td>

<td style="text-align:right;">

1.3012894

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

18.929557

</td>

<td style="text-align:right;">

18.929557

</td>

<td style="text-align:right;">

12.407185

</td>

<td style="text-align:right;">

160.89258

</td>

<td style="text-align:right;">

0.0649024

</td>

</tr>

<tr>

<td style="text-align:left;">

CH4.t.CO2e

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.5452572

</td>

<td style="text-align:right;">

5.9006979

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

108.460987

</td>

<td style="text-align:right;">

108.460987

</td>

<td style="text-align:right;">

15.916464

</td>

<td style="text-align:right;">

278.74807

</td>

<td style="text-align:right;">

0.2943001

</td>

</tr>

<tr>

<td style="text-align:left;">

HFC.PFCs.t.CO2e

</td>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0219541

</td>

<td style="text-align:right;">

0.2509943

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

4.161163

</td>

<td style="text-align:right;">

4.161163

</td>

<td style="text-align:right;">

13.617934

</td>

<td style="text-align:right;">

201.14719

</td>

<td style="text-align:right;">

0.0125185

</td>

</tr>

<tr>

<td style="text-align:left;">

Fugitive.kg

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.1221874

</td>

<td style="text-align:right;">

0.9156211

</td>

<td style="text-align:right;">

0.0001560

</td>

<td style="text-align:right;">

0.0036797

</td>

<td style="text-align:right;">

0.0002313

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

12.189404

</td>

<td style="text-align:right;">

12.189404

</td>

<td style="text-align:right;">

11.026986

</td>

<td style="text-align:right;">

130.43964

</td>

<td style="text-align:right;">

0.0456670

</td>

</tr>

<tr>

<td style="text-align:left;">

Stack.kg

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.4206834

</td>

<td style="text-align:right;">

2.7331204

</td>

<td style="text-align:right;">

0.0003090

</td>

<td style="text-align:right;">

0.0090083

</td>

<td style="text-align:right;">

0.0004581

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

33.321666

</td>

<td style="text-align:right;">

33.321666

</td>

<td style="text-align:right;">

8.712720

</td>

<td style="text-align:right;">

82.72182

</td>

<td style="text-align:right;">

0.1363157

</td>

</tr>

<tr>

<td style="text-align:left;">

Total.Air.kg

</td>

<td style="text-align:right;">

15

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.5428712

</td>

<td style="text-align:right;">

3.4459035

</td>

<td style="text-align:right;">

0.0005675

</td>

<td style="text-align:right;">

0.0135676

</td>

<td style="text-align:right;">

0.0008414

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

37.092916

</td>

<td style="text-align:right;">

37.092916

</td>

<td style="text-align:right;">

8.377980

</td>

<td style="text-align:right;">

73.86411

</td>

<td style="text-align:right;">

0.1718660

</td>

</tr>

<tr>

<td style="text-align:left;">

Surface.water.kg

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.1281632

</td>

<td style="text-align:right;">

1.0648699

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0003837

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

16.404607

</td>

<td style="text-align:right;">

16.404607

</td>

<td style="text-align:right;">

12.738129

</td>

<td style="text-align:right;">

175.01346

</td>

<td style="text-align:right;">

0.0531109

</td>

</tr>

<tr>

<td style="text-align:left;">

U\_ground.Water.kg

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0643912

</td>

<td style="text-align:right;">

0.5634316

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

8.839104

</td>

<td style="text-align:right;">

8.839104

</td>

<td style="text-align:right;">

12.131672

</td>

<td style="text-align:right;">

164.85908

</td>

<td style="text-align:right;">

0.0281014

</td>

</tr>

<tr>

<td style="text-align:left;">

Land.kg

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.3012675

</td>

<td style="text-align:right;">

3.2804508

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0003429

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

59.730055

</td>

<td style="text-align:right;">

59.730055

</td>

<td style="text-align:right;">

15.784280

</td>

<td style="text-align:right;">

271.04069

</td>

<td style="text-align:right;">

0.1636140

</td>

</tr>

<tr>

<td style="text-align:left;">

Offiste.kg

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.1176076

</td>

<td style="text-align:right;">

0.9913416

</td>

<td style="text-align:right;">

0.0000885

</td>

<td style="text-align:right;">

0.0029975

</td>

<td style="text-align:right;">

0.0001312

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

18.394299

</td>

<td style="text-align:right;">

18.394299

</td>

<td style="text-align:right;">

16.006124

</td>

<td style="text-align:right;">

285.75539

</td>

<td style="text-align:right;">

0.0494436

</td>

</tr>

<tr>

<td style="text-align:left;">

POTW.Metal.kg

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

402

</td>

<td style="text-align:right;">

0.0007064

</td>

<td style="text-align:right;">

0.0036614

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.0000332

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.040218

</td>

<td style="text-align:right;">

0.040218

</td>

<td style="text-align:right;">

7.481724

</td>

<td style="text-align:right;">

61.88361

</td>

<td style="text-align:right;">

0.0001826

</td>

</tr>

</tbody>

</table>

# Clustering for the data

# Fit Models

## user-define function

``` r
# test: target_nm = "CO.t"
makedata_map <- function(target_nm, dat){
  # Input columns
  Xy = dat %>%
    select(Coal.TJ, NatGase.TJ, Petrol.TJ, 
           Bio.Waste.TJ, NonFossElec.TJ, 
           Water.Withdrawals.Kgal,
           target_nm)
  # retin all inf by log(x + min/100)
  #Xy <- cbind(dat %>% select(Sector) %>% mutate(Sector= Sector %>% as.factor()), log10(Xy + min(Xy[Xy!=0])/100)) 
  # remove all inf= log(0)
  Xy <- cbind(dat %>% select(Sector), log(Xy)) 
  Xy <- Xy[!is.infinite(rowSums(Xy)),] %>% mutate(Sector= Sector %>% as.factor())
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
}
bind_coef_star <- function(x) {
  if (stringr::str_detect(x[2] , "\\*")) {
    paste0(x[1], "(",x[2], ")")
  } else if (!is.na(x[1])){
    paste0(x[1])
  } else{
    ""
  }
}
```

## Linear Regression Modeling with log10 Transformation on Input and Output

``` r
# create a dataframe with a column with impact variable names 
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list
#> # A tibble: 20 x 1
#>    target            
#>    <chr>             
#>  1 CO.t              
#>  2 NH3.t             
#>  3 NOx.t             
#>  4 PM10.t            
#>  5 PM2.5.t           
#>  6 SO2.t             
#>  7 VOC.t             
#>  8 Total.t.CO2e      
#>  9 CO2.Fossil.t.CO2e 
#> 10 CO2.Process.t.CO2e
#> 11 CH4.t.CO2e        
#> 12 HFC.PFCs.t.CO2e   
#> 13 Fugitive.kg       
#> 14 Stack.kg          
#> 15 Total.Air.kg      
#> 16 Surface.water.kg  
#> 17 U_ground.Water.kg 
#> 18 Land.kg           
#> 19 Offiste.kg        
#> 20 POTW.Metal.kg
# model selection for each impact variable
bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) %>% 
  mutate(top_model = data %>% 
           map(function(data) bestglm::bestglm(Xy=data %>% select_if(is.numeric), family = gaussian, method = "exhaustive", IC = "BIC", TopModels = 1))) %>% 
  mutate(best_model = top_model %>% map(function(top_model) top_model[[1]])) %>% 
  mutate(anv = best_model %>% map(anova)) %>% 
  mutate(statisics = best_model %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics)
# extract coefficient from the best model of each impact variable
coef_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, estimate) %>% 
  tidyr::spread(key= term, value = estimate)
# extract p-value for each paramters of the best model of each impact variable
signif_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, p.value) %>% 
  tidyr::spread(key= term, value = p.value)
# combind coefficient and p-value to result in a tidy table of the result
datArray <- abind::abind(coef_list %>% 
                           select(-target) %>% 
                           mutate_if(is.numeric, signif, digits = 3) %>% 
                           mutate_all(as.character),
                         signif_list %>% 
                           select(-target) %>% 
                           mutate_if(is.numeric, gtools::stars.pval),along=3)
coef_signif_list <- bestglm_list %>% 
  select(target, r.squared, adj.r.squared, p.value) %>% 
  cbind(apply(datArray,1:2, bind_coef_star) %>% as_tibble())
# result
coef_signif_list %>% arrange(desc(p.value)) %>% knitr::kable()
```

<table>

<thead>

<tr>

<th style="text-align:left;">

target

</th>

<th style="text-align:right;">

r.squared

</th>

<th style="text-align:right;">

adj.r.squared

</th>

<th style="text-align:right;">

p.value

</th>

<th style="text-align:left;">

(Intercept)

</th>

<th style="text-align:left;">

BioWasteTJ

</th>

<th style="text-align:left;">

CoalTJ

</th>

<th style="text-align:left;">

NatGaseTJ

</th>

<th style="text-align:left;">

NonFossElecTJ

</th>

<th style="text-align:left;">

PetrolTJ

</th>

<th style="text-align:left;">

WaterWithdrawalsKgal

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

POTW.Metal.kg

</td>

<td style="text-align:right;">

0.4867847

</td>

<td style="text-align:right;">

0.4817033

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-1.03(\*)

</td>

<td style="text-align:left;">

0.215(\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.755(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Land.kg

</td>

<td style="text-align:right;">

0.4658109

</td>

<td style="text-align:right;">

0.4611250

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

5.65(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.9(\*\*\*)

</td>

<td style="text-align:left;">

0.249(\*\*)

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Offiste.kg

</td>

<td style="text-align:right;">

0.4845963

</td>

<td style="text-align:right;">

0.4806316

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

5.8(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.165(\*\*\*)

</td>

<td style="text-align:left;">

0.707(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.204(\*\*\*)

</td>

<td style="text-align:left;">

\-0.0377(\*)

</td>

</tr>

<tr>

<td style="text-align:left;">

Surface.water.kg

</td>

<td style="text-align:right;">

0.6229475

</td>

<td style="text-align:right;">

0.6194563

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

2.97(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

1.5(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Stack.kg

</td>

<td style="text-align:right;">

0.5949768

</td>

<td style="text-align:right;">

0.5920418

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

0.945(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.533(\*\*\*)

</td>

<td style="text-align:left;">

0.166(\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.35(\*\*\*)

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Fugitive.kg

</td>

<td style="text-align:right;">

0.6404521

</td>

<td style="text-align:right;">

0.6378467

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-2.31(\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.89(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Total.Air.kg

</td>

<td style="text-align:right;">

0.6697148

</td>

<td style="text-align:right;">

0.6649625

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

5.2(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

1.23(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

NH3.t

</td>

<td style="text-align:right;">

0.7683303

</td>

<td style="text-align:right;">

0.7642299

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

5.29(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.141(\*\*\*)

</td>

<td style="text-align:left;">

0.68(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.195(\*\*\*)

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

VOC.t

</td>

<td style="text-align:right;">

0.6905785

</td>

<td style="text-align:right;">

0.6862205

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

2.74(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

1.07(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

CO.t

</td>

<td style="text-align:right;">

0.7511409

</td>

<td style="text-align:right;">

0.7476604

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-0.948(\*)

</td>

<td style="text-align:left;">

0.275(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.676(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

PM2.5.t

</td>

<td style="text-align:right;">

0.8781678

</td>

<td style="text-align:right;">

0.8745845

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-4.38(\*\*\*)

</td>

<td style="text-align:left;">

0.237(\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.725(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

PM10.t

</td>

<td style="text-align:right;">

0.8891458

</td>

<td style="text-align:right;">

0.8860010

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

0.842

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

0.847(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

NOx.t

</td>

<td style="text-align:right;">

0.9117863

</td>

<td style="text-align:right;">

0.9099226

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

3.39(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

1.1(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

SO2.t

</td>

<td style="text-align:right;">

0.9283235

</td>

<td style="text-align:right;">

0.9267985

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-0.76(\*\*)

</td>

<td style="text-align:left;">

0.18(\*\*\*)

</td>

<td style="text-align:left;">

0.172(\*\*)

</td>

<td style="text-align:left;">

0.636(\*\*\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Total.t.CO2e

</td>

<td style="text-align:right;">

0.9828135

</td>

<td style="text-align:right;">

0.9823259

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-1.6(\*\*\*)

</td>

<td style="text-align:left;">

0.244(\*\*\*)

</td>

<td style="text-align:left;">

0.334(\*\*\*)

</td>

<td style="text-align:left;">

0.573(\*\*\*)

</td>

<td style="text-align:left;">

\-0.229(\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

CO2.Fossil.t.CO2e

</td>

<td style="text-align:right;">

0.9910351

</td>

<td style="text-align:right;">

0.9908457

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

\-2.48(\*\*\*)

</td>

<td style="text-align:left;">

0.262(\*\*\*)

</td>

<td style="text-align:left;">

0.237(\*\*\*)

</td>

<td style="text-align:left;">

0.658(\*\*\*)

</td>

<td style="text-align:left;">

\-0.252(\*)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

</tbody>

</table>

# Communciate and visualize the results

## The linear model with R^2 \>0.75

``` r
good_lm <- bestglm_list %>% filter(adj.r.squared >0.75); good_lm %>% arrange(desc(adj.r.squared))
#> # A tibble: 7 x 16
#>   target data  top_model best_model anv   r.squared adj.r.squared sigma
#>   <chr>  <lis> <list>    <list>     <lis>     <dbl>         <dbl> <dbl>
#> 1 CO2.F… <df[… <bestglm> <lm>       <df[…     0.991         0.991 0.243
#> 2 Total… <df[… <bestglm> <lm>       <df[…     0.983         0.982 0.347
#> 3 SO2.t  <df[… <bestglm> <lm>       <df[…     0.928         0.927 0.803
#> 4 NOx.t  <df[… <bestglm> <lm>       <df[…     0.912         0.910 0.774
#> 5 PM10.t <df[… <bestglm> <lm>       <df[…     0.889         0.886 0.885
#> 6 PM2.5… <df[… <bestglm> <lm>       <df[…     0.878         0.875 0.898
#> 7 NH3.t  <df[… <bestglm> <lm>       <df[…     0.768         0.764 1.21 
#> # … with 8 more variables: statistic <dbl>, p.value <dbl>, df <int>,
#> #   logLik <dbl>, AIC <dbl>, BIC <dbl>, deviance <dbl>, df.residual <int>
```

## The diagnosis of linear model

``` r
par(mfrow=c(2,3))
plot(good_lm$best_model[[1]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
plot(good_lm$best_model[[2]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-2.png" width="100%" />

``` r
plot(good_lm$best_model[[3]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-3.png" width="100%" />

``` r
plot(good_lm$best_model[[4]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-4.png" width="100%" />

``` r
plot(good_lm$best_model[[5]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-5.png" width="100%" />

``` r
plot(good_lm$best_model[[6]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-6.png" width="100%" />

``` r
plot(good_lm$best_model[[7]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-7.png" width="100%" />

# Result

## Impact between Sector

``` r
# descriptive analysis
good_lm$target
#> [1] "NH3.t"             "NOx.t"             "PM10.t"           
#> [4] "PM2.5.t"           "SO2.t"             "Total.t.CO2e"     
#> [7] "CO2.Fossil.t.CO2e"
# NOx.t: Emissions of Nitrogen Oxides to Air from each sector. t = meric tons
# SO2.t: Emissions of Sulfur Dioxide to Air from each sector. t = meric tons 
# Total.t.CO2e: Global Warming Potential (GWP) is a weighting of greenhouse gas emissions into the air from the production of each sector. Weighting factors are 100-year GWP values from the IPCC Second Assessment Report (IPCC 2001). t CO2e = metric tons of CO2 equivalent emissions. 
# CO2.Fossil.t.CO2e C: Emissions of Carbon Dioxide (CO2) into the air from each sector from fossil fuel combustion sources. t CO2e = metric tons of CO2 equivalent.
par(mfrow=c(1,2))
plot(good_lm$data[[1]][,ncol(good_lm$data[[1]])])
plot(good_lm$data[[1]][,1], good_lm$data[[1]][,ncol(good_lm$data[[1]])])
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
plot(good_lm$data[[2]][,ncol(good_lm$data[[2]])])
plot(good_lm$data[[2]][,1], good_lm$data[[2]][,ncol(good_lm$data[[2]])])
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

``` r
plot(good_lm$data[[3]][,ncol(good_lm$data[[3]])])
plot(good_lm$data[[3]][,1], good_lm$data[[3]][,ncol(good_lm$data[[3]])])
```

<img src="man/figures/README-unnamed-chunk-10-3.png" width="100%" />

``` r
plot(good_lm$data[[4]][,ncol(good_lm$data[[4]])])
plot(good_lm$data[[4]][,1], good_lm$data[[4]][,ncol(good_lm$data[[4]])])
```

<img src="man/figures/README-unnamed-chunk-10-4.png" width="100%" />
