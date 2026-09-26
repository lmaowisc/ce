"""Editable vector figures for Chapter 1. Run from the repository root."""
from pathlib import Path
from html import escape
import re
import xml.etree.ElementTree as ET
OUT=Path(__file__).resolve().parents[1]/'figures'
OUT.mkdir(exist_ok=True)
INK='#332d34'; PLUM='#51334d'; COPPER='#9b573d'; LINE='#ded5da'; BG='#fffefd'
def text(x,y,s,size=21,anchor='middle',color=INK):
    return f'<text x="{x}" y="{y}" text-anchor="{anchor}" fill="{color}" font-size="{size}">{escape(str(s))}</text>'
def line(x,y,X,Y,color=INK,dash='',arrow=False):
    return f'<path d="M{x},{y} L{X},{Y}" fill="none" stroke="{color}" stroke-width="{1 if color == LINE else 2}"'+(f' stroke-dasharray="{dash}"' if dash else '')+(' marker-end="url(#arrow)"' if arrow else '')+'/>'
def box(x,y,w,h,label):
    return f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="4" fill="#f4edf2" stroke="{PLUM}" stroke-width="1.5"/>'+''.join(text(x+w/2,y+27+j*25,t) for j,t in enumerate(label.split('|')))
def mark(x,y,kind,color=INK):
    if kind=='death': return f'<circle cx="{x}" cy="{y}" r="7" fill="{color}"/>'
    if kind=='square': return f'<rect x="{x-7}" y="{y-7}" width="14" height="14" fill="{color}"/>'
    return f'<path d="M{x},{y-8} l8,8 -8,8 -8,-8 Z" fill="{BG}" stroke="{color}" stroke-width="2"/>'
def save(name,h,parts,title):
    svg=f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 {h}" role="img" aria-labelledby="title"><title id="title">{escape(title)}</title><defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 L10 5 L0 10 Z" fill="{INK}"/></marker></defs><rect width="640" height="{h}" fill="{BG}"/><g font-family="Georgia, Times New Roman, serif">'+''.join(parts)+'</g></svg>'
    # Shared book figure typography: Georgia 16 px at the 640 px reading width.
    # Tighten vertical layout without stretching letters or event symbols.
    root = ET.fromstring(svg)
    root.set('viewBox', f'0 0 640 {h * .82:g}')
    for node in root.iter():
        if node.tag.endswith('marker') or node.tag.endswith('defs'):
            continue
        for attr in ('y', 'cy'):
            if attr in node.attrib:
                node.set(attr, str(float(node.get(attr)) * .82))
        if 'height' in node.attrib:
            height = float(node.get('height'))
            if height != 14:
                node.set('height', str(height * .82))
            elif 'y' in node.attrib:
                node.set('y', str(float(node.get('y')) - 1.26))
        if 'font-size' in node.attrib:
            node.set('font-size', '16')
        if 'd' in node.attrib and ',' in node.get('d'):
            node.set('d', re.sub(r'([ML])([0-9.]+),([0-9.]+)',
                lambda m: f'{m[1]}{m[2]},{float(m[3]) * .82:g}', node.get('d')))
        if 'transform' in node.attrib:
            node.set('transform', re.sub(r'translate\(([0-9.]+),([0-9.]+)\)',
                lambda m: f'translate({m[1]},{float(m[2]) * .82:g})', node.get('transform')))
    ET.register_namespace('', 'http://www.w3.org/2000/svg')
    (OUT/name).write_text(ET.tostring(root, encoding='unicode'), encoding='utf-8')

save('colon-states.svg',280,[box(20,30,185,55,'Remission'),box(435,30,185,55,'Relapse'),box(230,190,180,55,'Death'),line(215,58,425,58,arrow=True),text(320,43,'296'),line(113,95,275,180,arrow=True),text(140,157,'33'),line(528,95,365,180,arrow=True),text(498,157,'258')],'Colon cancer: 296 relapses, 33 deaths without relapse, and 258 deaths after relapse')
save('hf-states.svg',320,[box(15,35,145,76,'Randomized|0 admissions'),box(225,35,160,76,'Hospitalized|1 admission'),box(485,35,145,76,'Hospitalized|26 admissions'),box(245,230,160,55,'Death'),line(168,73,215,73,arrow=True),text(188,29,'315',19),line(393,73,476,73,arrow=True),text(434,54,'…',27),text(434,20,'707 recurrent',17),line(88,122,255,221,arrow=True),text(143,194,'11'),line(305,122,365,175),line(557,122,365,175),line(365,175,345,221,arrow=True),text(480,213,'82 after admission',17)],'Hospitalization counts and death in HF-ACTION')
for kind in ['first','total']:
    p=[text(20,36,'Death weight = 2; hospitalization weight = 1',20,'start')]
    for y,label,end in [(105,'A',350),(195,'B',485)]:
        p += [text(30,y+7,label),line(65,y,350 if kind=='first' and label=='B' else end,y),mark(end,y,'death','#a99ca5' if kind=='first' and label=='B' else INK)]
    p += [mark(350,195,'hosp'),text(558,112,'2 units'),text(558,202,'1 unit' if kind=='first' else '3 units'),mark(85,277,'death'),text(106,284,'Death',19,'start'),mark(320,277,'hosp'),text(342,284,'Hospitalization',19,'start')]
    if kind=='first': p += [line(359,195,479,195,color='#a99ca5',dash='5 4'),text(410,232,'Later death omitted',16)]
    else:p += [text(390,234,'1 + 2 = 3',18)]
    save(kind+'-weight.svg',315,p,'First-event weighting' if kind=='first' else 'Weighted total-event count')

p=[mark(130,32,'death','#a85d60'),text(150,39,'Nonfatal event',19,'start'),mark(405,32,'square',PLUM),text(425,39,'Death',19,'start')]
x=lambda t:70+t*106
for t in range(6):p += [line(x(t),73,x(t),229,color=LINE),text(x(t),260,t,19)]
for y,label,death,color in [(110,'B',3.5,'#50758a'),(195,'A',4.5,'#a85d60')]:
    p += [text(30,y+7,label),line(x(0),y,x(death),y,color=color),mark(x(death),y,'square',color)]
p += [mark(x(2),195,'death','#a85d60')]
for t in [1,3,5]:p += [line(x(t),73,x(t),229,color=INK,dash='5 5')]
p += [text(320,295,'Time',20),text(320,335,'At 1: tie   ·   At 3: B wins   ·   At 5: A wins',20)]
save('changing-wins.svg',360,p,'Win-loss status changes with comparison time')

# Estimates and confidence limits transcribed from the supplied EMPA-REG figure.
rows=[('3-point MACE',.86,.74,.99,'#50758a'),('CV death',.62,.49,.78,'#956c86'),('Nonfatal MI',.87,.70,1.09,'#956c86'),('Nonfatal stroke',1.24,.92,1.67,'#956c86')]
p=[text(20,30,'Hazard ratio (95% confidence interval)',22,'start')]
x=lambda v:205+(v-.4)/1.4*410
for v in [.5,1,1.5]:p += [line(x(v),60,x(v),292,color=LINE,dash='4 4' if v==1 else ''),text(x(v),326,v,19)]
for j,(label,est,lo,hi,color) in enumerate(rows):
    y=88+j*62
    p += [text(15,y+6,label,20,'start'),line(x(lo),y,x(hi),y,color),line(x(lo),y-8,x(lo),y+8,color),line(x(hi),y-8,x(hi),y+8,color),mark(x(est),y,'death'),text(x(est),y+27,f'{est:.2f} ({lo:.2f}, {hi:.2f})',16)]
save('empa-components.svg',350,p,'EMPA-REG component hazard ratios')

# Approximate values read from the supplied raster, authorized by the author.
# These data are for the historical overview, not an updated registry search.
years=[2005,2012,2013,2014,2016,2017,2018,2019,2020,2021,2022,2023,2024]
trials_primary=[1,2,4,1,3,4,3,5,15,10,13,17,6]
trials_other=[0,1,2,1,0,0,2,7,3,3,2,6,1]
patients_primary=[100,500,800,1100,700,600,900,5600,15400,3200,8600,5500,4900]
patients_other=[0,800,200,1000,0,0,1900,3800,1200,4100,15400,12800,1000]
red='#a85d60';teal='#648b89'
p=[text(20,30,'Registered trials by start year',23,'start'),text(20,58,'Win ratio or hierarchical composite endpoint',18,'start'),f'<rect x="22" y="79" width="15" height="15" fill="{red}"/>',text(46,93,'Primary endpoint',18,'start'),f'<rect x="305" y="79" width="15" height="15" fill="{teal}"/>',text(329,93,'Secondary / other',18,'start')]
for top,maximum,step,a,b,title in [(152,25,5,trials_primary,trials_other,'Number of trials'),(465,25000,5000,patients_primary,patients_other,'Number of patients')]:
    bottom=top+215;p += [text(20,top-16,title,21,'start')]
    for v in range(0,maximum+1,step):
        y=bottom-v/maximum*215;p += [line(82,y,625,y,color=LINE),text(71,y+6,f'{v:,}',17,'end')]
    for j,year in enumerate(years):
        xx=88+(year-2005)*27;ha=a[j]/maximum*215;hb=b[j]/maximum*215
        p += [f'<rect x="{xx}" y="{bottom-ha}" width="19" height="{ha}" fill="{red}"/><rect x="{xx}" y="{bottom-ha-hb}" width="19" height="{hb}" fill="{teal}"/>',f'<text transform="translate({xx+9},{bottom+17}) rotate(-50)" text-anchor="end" font-size="16" fill="{INK}">{year}</text>']
p += [text(20,761,'Counts approximated from the graphical summary.',16,'start'),text(20,786,'ClinicalTrials.gov overview, December 2023.',16,'start')]
save('trial-uptake.svg',810,p,'Historical trial and participant counts, approximately digitized')
