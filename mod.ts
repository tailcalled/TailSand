title "Default Mod";

tag Empty "(all)";

relation Solid/1;

where Solid 'x:
	tag 'x "Solids";
	density 'x 1.0;
	description 'x "Solid. ";
end;

element Wall #808080;
tag Wall "(all)";
Solid Wall;

relation Dust/1;

where Dust 'x:
	tag 'x "Dusts";
	gravity 'x 1.0;
	spread 'x 0.1;
	density 'x 0.9;
	description 'x "Falls. ";
end;

element Sand #D8E090;
tag Sand "(all)";
Dust Sand;

relation Liquid/1;

where Liquid 'x:
	tag 'x "Liquids";
	gravity 'x 1.0;
	spread 'x 0.3;
	slide 'x 1.0;
	density 'x 0.8;
	description 'x "Liquid. ";
end;

element Water #0000FF;
tag Water "(all)";
Liquid Water;

relation Firey/2;

where Firey 'elem 'extinguished:
	description 'elem "Burns. ";
end;

element Fire1 #FFDF00;
tag Fire1 "(all)";
name Fire1 "Fire";
Firey Fire1 Empty;
gravity Fire1 -0.0;
density Fire1 0.5;
element Fire2 #FF8000;
Firey Fire2 Empty;
gravity Fire2 -0.5;
density Fire2 0.51;
element Fire3 #FF0000;
Firey Fire3 Empty;
gravity Fire3 -1.0;
density Fire3 0.52;
self 0.2 Fire1 => Fire2;
self 0.2 Fire2 => Fire3;
self 0.2 Fire3 => Empty;

relation Flammable/1;

where Flammable 'x:
	tag 'x "Flammables";
	description 'x "Flammable. ";
end;

where Flammable 'fuel, Firey 'fire 'extinguished:
	reaction 1.0 'fuel 'fire => Fire1 'fire;
end;

element Oil #804000;
tag Oil "(all)";
Liquid Oil;
density Oil 0.7;
Flammable Oil;

element Plant #008000;
tag Plant "(all)";
Solid Plant;
Flammable Plant;
reaction 1.0 Water Plant => Plant Plant;

relation Source/2;

where Source 'source 'elem:
	tag 'source "Sources";
	Solid 'source;
	reaction 1.0 'source Empty => 'source 'elem;
end;

element Spring #000080;
tag Spring "(all)";
Source Spring Water;

element Nature #808000;
tag Nature "(all)";
Source Nature Plant;

element OilWell #603000;
tag OilWell "(all)";
name OilWell "Oil Well";
Source OilWell Oil;

element Torch #800000;
tag Torch "(all)";
Source Torch Fire1;
Firey Torch Torch;

element Beach #FFFF00;
tag Beach "(all)";
Source Beach Sand;

relation Gas/1;

where Gas 'x:
	description 'x "Gas. ";
	tag 'x "Gasses";
	gravity 'x -1;
	spread 'x 0.1;
	density 'x -0.9;
	slide 'x 1.0;
end;

element Steam #E0E0E0;
Gas Steam;
self 0.05 Steam => Water;

where Firey 'fire 'extinguished:
	reaction 0.1 Water 'fire => Steam 'fire;
	reaction 0.4 Water 'fire => Steam 'extinguished;
	reaction 0.5 Water 'fire => Water 'extinguished;
end;