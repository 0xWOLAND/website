import plotly.graph_objects as go
import pandas as pd

C = { # [tech_diff, autonomy, wealth, scale]
    "Founder (VC-backed)":      [6,9,7,10],  "Founder (bootstrapped)": [6,10,6,5],
    "Quant / HFT":              [10,3,10,7], "FAANG SWE":              [7,2,8,8],
    "Staff+ Engineer":          [8,4,9,9],   "Infra/Platform (big co)":[9,3,8,10],
    "ML Researcher (industry)": [10,5,8,7],  "ML Researcher (academia)":[10,7,3,5],
    "Solo Dev / Indie Hacker":  [5,10,4,3],  "Open Source Maintainer":  [7,9,3,8],
    "VC Partner":               [3,6,9,8],   "Management Consulting":   [2,2,8,7],
    "Investment Banking":       [2,1,9,9],   "PM at Big Tech":          [3,3,8,7],
    "CTO / VP Eng":             [7,7,9,9],   "Deep Tech Founder":       [10,9,5,9],
    "Govt / Defense Tech":      [8,3,6,9],   "DevTools Founder":        [9,9,6,8],
    "Crypto/Web3 Builder":      [7,8,6,7],   "Technical Writer":        [3,5,4,2],
    "Data Engineer":            [7,3,7,8],   "Security Researcher":     [9,5,7,5],
    "Robotics Engineer":        [9,4,7,6],   "Bio/Hardware Startup":    [10,8,4,8],
}

df = pd.DataFrame(C, index=["tech","auto","wealth","scale"]).T
df["fit"] = (df["scale"] + df["auto"] + df["wealth"]) / 3
v = (df["wealth"] >= 5) & (df["auto"] >= 5)

def tr(s, op, sc):
    return go.Scatter3d(
        x=s["tech"], y=s["auto"], z=s["scale"], mode="markers+text",
        marker=dict(size=s["fit"]*2.5, color=s["wealth"], colorscale="Greens",
                    cmin=1, cmax=10, opacity=op, showscale=sc,
                    colorbar=dict(title="Wealth"), line=dict(width=1, color="black")),
        text=s.index, textposition="top center", textfont=dict(size=9),
        hovertemplate="<b>%{text}</b><br>Tech: %{x} Auto: %{y} Scale: %{z}<br>"
                      "Wealth: %{customdata[0]} Fit: %{customdata[1]:.1f}<extra></extra>",
        customdata=s[["wealth","fit"]].values)

fig = go.Figure([tr(df[~v], .12, False), tr(df[v], .9, True)])
fig.update_layout(
    title="Career space — top-right-back corner is the goal",
    scene=dict(xaxis_title="Tech Difficulty →", yaxis_title="Autonomy →",
               zaxis_title="Scale →", **{f"{a}axis": dict(range=[0,11]) for a in "xyz"}),
    font=dict(family="IBM Plex Mono"), width=1000, height=750,
    margin=dict(l=0, r=0, t=40, b=0))
fig.show()
