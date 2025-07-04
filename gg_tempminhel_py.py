import pandas as pd
from io import StringIO
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Patch

# ——— Datos de heladas —————————————————————————————
heladas_txt = """month,year,Heladas
6,2018,4
7,2018,1
8,2018,3
9,2018,0
6,2019,1
7,2019,3
8,2019,2
9,2019,1
6,2020,2
7,2020,8
8,2020,4
9,2020,1
6,2021,2
7,2021,3
8,2021,1
9,2021,0
"""
# ——— Datos de temp mínima media ——————————————————————————
min_txt = """month,year,Mean_min
1,2018,18.183871
1,2019,19.074194
1,2020,17.8
1,2021,18.825806
2,2018,18.214286
2,2019,16.557143
2,2020,16.558621
2,2021,16.907143
3,2018,13.941935
3,2019,14.683871
3,2020,17.735484
3,2021,15.993548
4,2018,17.066667
4,2019,11.88
4,2020,11.38
4,2021,14.13
5,2018,11.33
5,2019,9.358065
5,2020,8.541935
5,2021,6.177419
6,2018,4.354167
6,2019,9.044828
6,2020,6.620690
6,2021,7.190909
7,2018,7.844444
7,2019,5.746154
7,2020,4.307692
7,2021,4.907143
8,2018,6.229167
8,2019,7.019231
8,2020,6.863333
8,2021,5.431034
9,2018,11.966667
9,2019,7.568966
9,2020,6.914286
9,2021,10.97
10,2018,15.322581
10,2019,15.23
10,2020,14.643333
10,2021,14.473333
11,2018,15.661290
11,2019,15.23
11,2020,14.643333
11,2021,14.473333
12,2018,15.322581
12,2019,15.661290
12,2020,15.483871
12,2021,18.583871
"""

# Leer desde strings
df_hel = pd.read_csv(StringIO(heladas_txt))
df_min = pd.read_csv(StringIO(min_txt))
df_hel['year'] = df_hel['year'].astype(str)
df_min['year'] = df_min['year'].astype(str)

# Pivot
pivot_min = df_min.pivot(index='month', columns='year', values='Mean_min')
pivot_hel = df_hel.pivot(index='month', columns='year', values='Heladas')

years = ['2018','2019','2020','2021']
colors = {'2018':'#1F78B4','2019':'#B88DB4','2020':'#B2DF8A','2021':'#FF7F00'}
width = 0.16
x = pivot_hel.index.values

# Letras de significancia para temp mínima
label_min = pd.DataFrame({
    "month": [3,3,3,3, 4,4,4,4, 5,5,5,5, 6,6,6,6, 9,9,9,9, 12,12,12,12],
    "year":  ["2020","2021","2019","2018"]*6,
    "group": ["a","ab","b","b",
              "c","b","bc","a",
              "bc","c","ab","a",
              "ab","ab","a","b",
              "b","a","b","a",
              "b","a","b","b"]
})
# Unir con Mean_min para la posición
label_min = label_min.merge(df_min[['month','year','Mean_min']],
                            on=['month','year'], how='left')

# Letras de significancia para heladas
labels_hel = {
    6: ["a","b","b","b"],
    7: ["c","b","a","b"],
    9: ["b","a","a","b"]
}

# ——— Graficado ————————————————————————————————————————————————
fig, ax1 = plt.subplots(figsize=(15,9))

# 1) Curvas de temp mínima
for yr in years:
    ax1.plot(pivot_min.index, pivot_min[yr],
             marker='o', color=colors[yr], linewidth=2)

# 2) Líneas de referencia
ax1.axhline(12.708, color="#993427", linestyle=':', linewidth=1.8)
ax1.axhline(12.5,   color="#1F78B4", linestyle='--', linewidth=1.5)
ax1.axhline(11.9,   color="#B88DB4", linestyle='--', linewidth=1.5)
ax1.axhline(11.4,   color="#B2DF8A", linestyle='--', linewidth=1.5)
ax1.axhline(12.0,   color="#FF7F00", linestyle='--', linewidth=1.5)

# 3) Letras de temp mínima
for _, row in label_min.iterrows():
    ax1.text(row['month'], row['Mean_min'] + 0.3, row['group'],
             color=colors[row['year']], ha='center', fontsize=23)

# 4) Ejes y grid
ax1.set_ylim(0,20)
ax1.set_yticks([0,5,10,15,20])
ax1.set_yticklabels([0,5,10,15,20], fontsize=23)
ax1.set_ylabel("Temperatura mínima (°C)", fontsize=28)
ax1.set_xticks(range(1,13))
ax1.set_xticklabels(
    ["Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"],
    fontsize=25
)
ax1.grid(axis='y', color='lightgray', linestyle='-', linewidth=0.8)
ax1.spines['top'].set_visible(False)

# 5) Barras de días con helada (mismo rango 0–20 y líneas a 5)
ax2 = ax1.twinx()
for i, yr in enumerate(years):
    ax2.bar(x + (i-1.5)*width, pivot_hel[yr],
            width=width, alpha=0.5, color=colors[yr])

    # Letras de heladas justo encima de cada barra
    for m in pivot_hel.index:
        lab = labels_hel.get(m, [None]*4)[i]
        if lab:
            ybar = pivot_hel.loc[m, yr]
            ax2.text(m + (i-1.5)*width, ybar + 0.4, lab,
                     color="black", ha='center', fontsize=23)

ax2.set_ylim(0,20)
ax2.set_yticks([0,5,10,15,20])
ax2.set_yticklabels([0,5,10,15,20], fontsize=23)
ax2.set_ylabel("Días con helada", fontsize=28)
ax2.spines['top'].set_visible(False)

from matplotlib.lines import Line2D

from matplotlib.lines import Line2D

# ... tu código de trazado de líneas y barras ...

# 6) Crear handles con cuadrados
handles = [
    Line2D([], [], marker='s', linestyle='None',
           markersize=15, markerfacecolor=colors[yr],
           markeredgecolor='none', label=yr)
    for yr in years
]

# Ajusta espacio inferior para que la leyenda quepa
fig.subplots_adjust(bottom=0.20)

# Añade la leyenda en ax2 (o ax1, da igual) un poco más arriba
ax2.legend(
    handles=handles,
    ncol=4,
    loc='upper center',
    bbox_to_anchor=(0.5, -0.10),  # subir de -0.15 a -0.10
    frameon=False,
    fontsize=25,
    title=None,
    columnspacing=0.1,
    handletextpad=0.2
)

# 7) Guardar en TIFF a 300 dpi
plt.savefig("c:/Users/um/Documents/ipovilonis/Doc/6 - Produccion/2025 geoespacial/gg_tmin_hel_py.tiff", format="tiff", dpi=300, bbox_inches="tight")
plt.show()
