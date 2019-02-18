program ArtilleryV;
uses {imagination} Swingame, sgTypes, SysUtils;
{
	Game in which players learn how to best shoot one another.
	Created by Jimmy Trac, ID:1016 24 964
}
Const
	SCREEN_HEIGHT=500;
	SCREEN_WIDTH=SCREEN_HEIGHT * 16 div 9;
	TEXTX=50;
	TEXTY=50;

Type
	HeightMap = Array[0..SCREEN_WIDTH] of Single;
	TerrainReduction = (Linear, Exponential);
//

function Power(base,expo:Double):Integer;
begin
	result:=Round(Exp(expo*Ln(base))); 
end;

function FPower(base,expo:Double):double; //floating point power
begin
	result:=Exp(expo*Ln(base)); 
end;

function Ceil(x:Double):Integer;
//Rounds a floating point number up.
begin
	result:=trunc(x)+1;
end;

function PowerCeil(base:Double;expo:Double):Integer;
//returns the overdetermined exponent to the base such that expo^result>=base
begin
	result:=ceil(ln(base)/ln(expo))
end;

function FRandBtw(min:Double; max: Double):Double;
//returns a floating point number between two others.
begin
	result:= min + (max - min) * rnd();
end;

function RandDisplacement(displacement:Integer):Integer;
//returns a value between [-displacement, displacement]
begin
	result:=round(rnd()*displacement * 2 - displacement);
end;

procedure GenerateTerrain(var groundMap:HeightMap; displacement:Integer; roughness:Double; reductionType:TerrainReduction);
var
	i,j,k,currentIndex,segments,segmentLength,terrainWidth,exponent:Integer;
	reduction:Double;
begin
	reduction:= 1 / fPower(2,roughness);
	
	exponent:=powerCeil(SCREEN_WIDTH,2);
	
	groundMap[0]:=SCREEN_HEIGHT/3 + randDisplacement(displacement);
	groundMap[SCREEN_WIDTH-1]:=SCREEN_HEIGHT/2 + randDisplacement(displacement);
	
	for i:=0 to exponent do
	begin
		segments:=power(2,i);
		segmentLength:=round((terrainWidth / segments) / 2);
		
		for k:=1 to segments do 
		begin
			currentIndex:= (k * segmentLength * 2) - segmentLength;
			FillCircle(ColorBlack,currentIndex,groundMap[currentIndex],2);
		end;
		
		
		for j:=1 to segments do 
		begin
			currentIndex:= (j * segmentLength * 2) - segmentLength; //segmentLength * 2 = distance between segment midpoints
			groundMap[currentIndex]:=(groundMap[currentIndex-segmentLength] + groundMap[currentIndex+segmentLength]) / 2;
			groundMap[currentIndex]:=groundMap[currentIndex] + randDisplacement(displacement);
		end;
		case reductionType of
			Linear: displacement:=round(displacement * reduction);
			Exponential: displacement:=trunc(power(displacement,reduction));
		end;
	end;
end;

procedure Main();
var
	groundMap:Heightmap;
begin
	OpenGraphicsWindow('Terrain Generation for ArtilleryV',SCREEN_WIDTH,SCREEN_HEIGHT);
	generateTerrain(groundMap,300,0.87,Linear);
	repeat
		ProcessEvents();
		RefreshScreen(120);
	until WindowCloseRequested();
end;
	
begin
	Main();
end.