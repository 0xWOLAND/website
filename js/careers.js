// [autonomy, wealth, scale]
const C={
  "Founder (VC-backed)":[9,7,10],"Founder (bootstrapped)":[10,6,5],
  "Quant / HFT":[3,10,7],"FAANG SWE":[2,8,8],
  "Staff+ Engineer":[4,9,9],"Infra/Platform (big co)":[3,8,10],
  "ML Researcher (industry)":[5,8,7],"ML Researcher (academia)":[7,3,5],
  "Solo Dev / Indie Hacker":[10,4,3],"Open Source Maintainer":[9,3,8],
  "VC Partner":[6,9,8],"Management Consulting":[2,8,7],
  "Investment Banking":[1,9,9],"PM at Big Tech":[3,8,7],
  "CTO / VP Eng":[7,9,9],"Deep Tech Founder":[9,5,9],
  "Govt / Defense Tech":[3,6,9],"DevTools Founder":[9,6,8],
  "Crypto/Web3 Builder":[8,6,7],"Technical Writer":[5,4,2],
  "Data Engineer":[3,7,8],"Security Researcher":[5,7,5],
  "Robotics Engineer":[4,7,6],"Bio/Hardware Startup":[8,4,8]
};
const names=Object.keys(C),vals=Object.values(C);
const auto=vals.map(v=>v[0]),wealth=vals.map(v=>v[1]),scale=vals.map(v=>v[2]);
const fit=vals.map(v=>(v[0]+v[1]+v[2])/3);
const viable=vals.map(v=>v[0]>=5&&v[1]>=5);
const mk=(idx,op,sc)=>({
  type:"scatter3d",mode:"markers+text",showlegend:false,
  x:idx.map(i=>auto[i]),y:idx.map(i=>wealth[i]),z:idx.map(i=>scale[i]),
  text:idx.map(i=>names[i]),textposition:"top center",textfont:{size:9},
  marker:{size:idx.map(i=>fit[i]*2.5),color:idx.map(i=>fit[i]),
    colorscale:"YlOrRd",cmin:3,cmax:9,opacity:op,
    showscale:sc,colorbar:{title:"Fit"},line:{width:1,color:"black"}},
  customdata:idx.map(i=>[auto[i],wealth[i],scale[i],fit[i]]),
  hovertemplate:"<b>%{text}</b><br>Autonomy: %{x} Wealth: %{y} Scale: %{z}<br>"+
    "Fit: %{customdata[3]:.1f}<extra></extra>"
});
const yes=[],no=[];
viable.forEach((v,i)=>(v?yes:no).push(i));
Plotly.newPlot("career-plot",[mk(no,.12,false),mk(yes,.9,true)],{
  title:"Career space — top-right-back is the goal",
  scene:{xaxis:{title:"Autonomy →",range:[0,11]},
         yaxis:{title:"Wealth →",range:[0,11]},
         zaxis:{title:"Scale →",range:[0,11]}},
  font:{family:"IBM Plex Mono"},margin:{l:0,r:0,t:40,b:0}
},{responsive:true});
