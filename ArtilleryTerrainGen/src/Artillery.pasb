program Artillery;
uses {imagination} Swingame, sgTypes, SysUtils, TerminalUserInput {and anime girls};
{
Comments from the Creator
None, for now.

Physical Quantities
Meters/Distance		-	Pixel(px)	= 1 Whole Number
Seconds/Time		-	Loop(l)		= Game loop, should be relatively stable.

To Do:
Explosion Animation
Stuff breaking off like in gunbound when shells hit the ground
AP shells either have better expl radius or homing, or both
nerf CGC
Checkbox Radio-button system so game remembers player's shell choices
Winning animation/system	
Arty Sprites

Armour -high-angle shots are more effective + high velocity does more damage


}
Const
	SCREEN_HEIGHT=500;
	SCREEN_WIDTH=SCREEN_HEIGHT * 16 div 9;
	TARGET_FPS=60;
	GRAVITY_ACC=0.3;
	VEHICLE_MAXSPEED=0.95;				//maximum speed units/loop
	VEHICLE_ACC=0.15;					//0.5 px/(l^2)
	VEHICLE_BRAKE=0.4;					//Acts as friction, 0~1 . Brakes always kept on to limit top speed of vehicles.
	VEHICLE_FRICTION=0.8;				//When brake is off
	VEHICLE_SLOPE_RADIUS=5;				//Radius to scan to determine the slope
	VEHICLE_SIZE=5;						//Size in pixels to scan and determine vehicle position
	VEHICLE_SLOPE_MULTIPLIER=0.5;		//Determines how strongly slopes affect acceleration
	VEHICLE_SCALE_FACTOR=0.1;			//Scale bitmap
	DEGREE_INCREMENT=0.5;				//Increment per loop, in degrees
	POWER_INCREMENT=5;
	POWER_TO_PXL=0.022;
	ARTY_DATA_FILE='data.ia';
	PROJECTILE_RADIUS=2;
	VELOCITY_LOSS_MULTIPLIER=0.7;		//How much to reduce loss by and increase distance
	MAX_PROJECTILES=15;
	MAX_TRACERS=10;
	TRACER_FADE_TIME=MAX_TRACERS;		//Number of loops before tracer disappears
	DAMAGE_FADE_TIME=100;
	DAMAGE_TEXT_SPEED=1;
	
Type
	HeightMap = Array of Single;
	SPGType = (GWTiger, CGC, OBJ);
	ShellType = (HE, HEAT, HESH, AP, APHE, APCR, ATGM); //High Explosive (High dmg, low pen), High-Explosive Anti-Tank (high pen, no dmg, no pen loss over distance), High-Explosive Squash-Head (averagepen, average damage), Armour Piercing (high pen, high damage, no expl.), Armour-Piercing High-Explosive (high pen, super-high damage, large pen loss over distance), Armour-Piercing Composite-Rigid (super-high pen, large pen loss over distance), Anti-Tank Guided Missile (sue me.).
	Team = (Red, Green, Blue, White, Black); //for MP
	TerrainType = (Hills, Plains, Mountains, Wasteland, Barren, Twilight);
	TerrainReduction = (Linear, Exponential);
	Direction = (Left, Right);
	
	
	//-------------------- Loaded with InitialiseData
	ShellData = Record
		Shell:ShellType;			//Load procedure takes care of this
		Name:String;
		MaxPenetration:Integer;		//in mm
		VelocityBleed:Single;		//between 0 and 1, nominally 0.9-0.99, HE loses more (0.9), HESH even more, APHE/AP not as much (0.95+), ATGMs >1
		Damage:Integer;
		PenetrationLoss:Boolean;	//Whether or not to lose peneration over distance. Only for HE/HESH/HEAT/ATGMs
		BlastRadius:Integer;			//Pixels, left and right. HE~30, AP~5, etc.
		IsGuided:Boolean;			//Determines whether or not we guide it
		TransferCoefficient:Single; //used for ATGMs, set to 0 for everything else, normally low, 0.1-0.4
	end;
	SPGSprite = Record //SPG Sprite Data
		HullFile:AnsiString;
		BarrelFile:AnsiString;
		GirlFile:AnsiString;
		HullBitmap:Bitmap;
		BarrelBitmap:Bitmap;
		GirlBitmap:Bitmap;
	end;
	SPGData = Record //The SPG 's data
		Model:SPGType;							//Load procedure takes care of this
		Name:String;
		MaxHP:Integer;
		TopArmour, FrontArmour, RearArmour:Integer;
		MaxGunVelocity:Single;					//in px/l
		GunElevation, GunDepression:Integer;	//in degrees
		ShellStat: Array of ShellData;
		Sprite:SPGSprite;
		TraverseLimit:Integer;					//in px
		MaxNumberShells:Integer;
	end;
	Map = Record
		Terrain: TerrainType;					//Load procedure takes care of this
		Name:String;
		Description:String;
		SkyColour:Color;
		C1,C2,C3:Color;
		MaxRoughness, MinRoughness:Single;
	end;
	//--------------------------------------------------
	
	Ammunition = Record			//This tells the game how many of each the player has left
		Shell:ShellType;		//The actual shell dynamics are taken from 
		Number:Integer;			//IAData.SPG[Integer(enum)].ShellStat[Integer(Enum)]
	end;
	
	TracerData = Record
		Exists:Boolean;
		Colour:Color;
		FadeTime:Integer;		//Number of loops before WE DESTROY THE TRACER
		x,y:Single;
	end;
	
	ProjectileData = Record
		Exists:Boolean;
		Owner:Integer;			//Player that fired the projectile
		Shell:ShellType;		//To determine damage
		Colour:Color;			//"Color" = Type, "Colour" = everything else
		x,y:Single;				//Originally used swingame, was useless.
		dx,dy:Single;			//velocity
		Tracer: Array[0..MAX_TRACERS-1] of TracerData;
	end;
	
	DamageTextData = Record
		Exists:Boolean;
		Value:String;
		x,y,dy:Single;
		FadeTime:Integer;
	end;
	
	PlayerData = Record
		Name: String;
		SPG:SPGType;
		HP:Integer;
		Exists:Boolean;
		AmmunitionRack: Array[0..Integer(High(ShellType))] of Ammunition;
		traverseLeft:Single;
		Credits:Integer;
		CurrentTeam:Team;
		Acc,Vel,Pos:Single;				//Acceleration, Velocity, X-Position
		Facing:Direction;				//Current vehicle facing
		GunAngle:Single;				//Current gun Angle; In radians
		VehicleAngle:Single;			//Angle of player's Vehicle on Terrain
		BrakesOn:Boolean;				//Whether or not brakes are on. Normally on.
		ProjectilePower:Single;			//for shooting
		FireProjectile:Boolean;
	end;
	
	MainGameData = Record
		Player: Array of PlayerData;
		SPG: Array of SPGData;
		Projectile: Array[0..MAX_PROJECTILES-1] of ProjectileData;
		DamageText: Array[0..MAX_PROJECTILES-1] of DamageTextData;
		ShellPanel:Panel;
		GroundMap: HeightMap;
		CurrentPlayer:Integer;
		ActiveShellIndex:Integer;
	end;

procedure DrawMap(groundMap:HeightMap);
var
	i:Integer;
begin
	ClearScreen(colorWhite);
	for i:=0 to SCREEN_WIDTH do
	begin
		DrawLine(colorGreen,i,SCREEN_HEIGHT-groundMap[i],i,SCREEN_HEIGHT);
	end;
end;

function Power(base,expo:Double):Integer;
begin
	result:=Round(Exp(expo*Ln(base))); 
end;

function FPower(base,expo:Double):double; //floating point power
begin
	result:=Exp(expo*Ln(base)); 
end;

function Ceil(x:Double):Integer;
begin
	result:=trunc(x)+1;
end;

function PowerCeil(base:Double;expo:Double):Integer;
//returns the overdetermined exponent to the base such that expo^result>=base
begin
	result:=ceil(ln(base)/ln(expo))
end;

function FRandBtw(min:Double; max: Double):Double;
begin
	result:= min + (max - min) * rnd();
end;

function RandDisplacement(displacement:Integer):Integer;
//returns a value between [-displacement, displacement]
begin
	result:=round(rnd()*displacement * 2 - displacement);
end;

procedure ClearTerrain(var groundMap:HeightMap);
var
	i:Integer;
begin
	for i:=Low(groundMap) to High(groundMap) do
	begin
		groundMap[i]:=0;
	end;
end;

function Rad(deg:Single):Single;
begin
	result:=deg * 2 * pi / 360;
end;

procedure GenerateTerrain(var groundMap:HeightMap; displacement:Integer; roughness:Double; reductionType:TerrainReduction);
var
	i,j,currentIndex,segments,segmentLength,terrainWidth,exponent:Integer;
	reduction:Double;
begin
	reduction:= 1 / fPower(2,roughness);
	
	exponent:=powerCeil(SCREEN_WIDTH,2);
	terrainWidth:=power(2,exponent); //SCREEN_WIDTH of 800 gives log_2 800 ~ 9.64... ~ 10. 2^10=1024
	setLength(groundMap, terrainWidth+1);
	
	groundMap[0]:=SCREEN_HEIGHT/2 + randDisplacement(displacement);
	groundMap[terrainWidth]:=SCREEN_HEIGHT/2 + randDisplacement(displacement);
	
	for i:=0 to exponent do
	begin
		segments:=power(2,i);
		segmentLength:=round((terrainWidth / segments) / 2);
		
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

function GroundPosition(x:Integer;IAData:MainGameData):Integer;
begin
	result:=Round(SCREEN_HEIGHT-IAData.GroundMap[x]);
end;

function ReadBooleanLine(var dataFile:TextFile):Boolean;
var
	readString:String;
begin
	ReadLn(dataFile,readString);
	if (readString = '1') then
		begin
			result:=true;
		end
		else
		begin
			result:=false;
		end;
end;

procedure InitialiseData(var IAData:MainGameData; dataLocation:String);
var
	dataFile: TextFile;
	arty,i:Integer;
begin
	WriteLn('Loading from ',dataLocation);
	AssignFile(dataFile,dataLocation);
	reset(dataFile);
	WriteLn('Data File file opened');
	ReadLn(dataFile); //First line is not important
	//readln (file, towhere)
	
	SetLength(IAData.SPG,Integer(High(SPGType))+1);
	for arty:=0 to Integer(High(SPGType)) do
	begin
		IAData.SPG[arty].Model:=SPGType(arty);
		ReadLn(dataFile,IAData.SPG[arty].Name);
		ReadLn(dataFile,IAData.SPG[arty].MaxHP);
		ReadLn(dataFile,IAData.SPG[arty].TopArmour);
		ReadLn(dataFile,IAData.SPG[arty].FrontArmour);
		ReadLn(dataFile,IAData.SPG[arty].RearArmour);
		ReadLn(dataFile,IAData.SPG[arty].MaxGunVelocity);
		ReadLn(dataFile,IAData.SPG[arty].GunElevation);
		ReadLn(dataFile,IAData.SPG[arty].GunDepression);
		//writeln('passed main, entering into loop for shells for ARTY: ',arty);
		SetLength(IAData.SPG[arty].ShellStat,Integer(High(ShellType))+1);
		for i:=0 to Integer(High(ShellType)) do
		begin
			ReadLn(dataFile);
			IAData.SPG[arty].ShellStat[i].Shell:=ShellType(i);
			ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].Name);
			ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].MaxPenetration);
			ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].VelocityBleed);
			ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].Damage);
			IAData.SPG[arty].ShellStat[i].PenetrationLoss:=ReadBooleanLine(dataFile);
			ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].BlastRadius);
			IAData.SPG[arty].ShellStat[i].IsGuided:=ReadBooleanLine(dataFile);
			IAData.SPG[arty].ShellStat[i].TransferCoefficient:=0;
			if (IAData.SPG[arty].ShellStat[i].IsGuided) then ReadLn(dataFile,IAData.SPG[arty].ShellStat[i].TransferCoefficient);
			//writeln('passed shell loop for ShellType: ',i);
		end;
		//writeln('passed shell loops.');
		ReadLn(dataFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.HullFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.BarrelFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.GirlFile);
		ReadLn(dataFile,IAData.SPG[arty].TraverseLimit);
		ReadLn(dataFile,IAData.SPG[arty].MaxNumberShells);
		ReadLn(dataFile);
		//writeln('end of loop.');
	end;
end;

procedure LoadResources(IAData:MainGameData);
var
	i:Integer;
begin
	for i:=0 to Integer(High(SPGType)) do
	begin
	LoadBitmapNamed(IAData.SPG[i].Sprite.HullFile,IAData.SPG[i].Sprite.HullFile);
	LoadBitmapNamed(IAData.SPG[i].Sprite.BarrelFile,IAData.SPG[i].Sprite.BarrelFile);
	LoadBitmapNamed(IAData.SPG[i].Sprite.GirlFile,IAData.SPG[i].Sprite.GirlFile);
	BitmapNamed(IAData.SPG[i].Sprite.HullFile);
	IAData.SPG[i].Sprite.BarrelBitmap:=BitmapNamed(IAData.SPG[i].Sprite.BarrelFile);
	IAData.SPG[i].Sprite.GirlBitmap:=BitmapNamed(IAData.SPG[i].Sprite.GirlFile);
	
	IAData.SPG[i].Sprite.HullBitmap:=RotateScaleBitmap(IAData.SPG[i].Sprite.HullBitmap,0,VEHICLE_SCALE_FACTOR);
	end;
end;

function RandomPlayer(IAData:MainGameData):Integer;
begin
	if (Length(IAData.Player)=1) then
	begin
		result:=0;
	end
	else
	begin
		result:=Rnd(High(IAData.Player));
	end;
end;

procedure SelectNumberPlayers(var IAData:MainGameData); //Will show a panel indicating number of players
var
	numberPlayers:Integer;
begin
	numberPlayers:=ReadInteger('Enter in the number of players: ');
	SetLength(IAData.Player,numberPlayers);
end;

procedure SelectVehicle(var player:PlayerData); //Select your girl now!
var
	selection:Integer;
	SelectedVehicle:SPGType;
begin
	WriteLn('Debug: Enter in enum index, pref. 0');
	selection:=ReadInteger('Vehicle: ');
	SelectedVehicle:=SPGType(selection);
	player.SPG:=SelectedVehicle;
end;

procedure ReadUserNames(var IAData:MainGameData);
var
	i:Integer;
	currentName:String;
begin
	for i:=0 to High(IAData.Player) do
	begin
		//Debug!
		WriteLn('What is Player ',i+1,'''s name?');
		IAData.Player[i].Name:=ReadString('Name: ');
	end;
end;

procedure SelectPlayerVehicles(var IAData:MainGameData);
var
	i:Integer;
begin
	for i:=Low(IAData.Player) to High(IAData.Player) do
	begin
		SelectVehicle(IAData.Player[i]);
	end;
end;

procedure NewGame(var IAData:MainGameData);
var
	i:Integer;
begin
	SelectNumberPlayers(IAData);
	ReadUserNames(IAData);
	SelectPlayerVehicles(IAData);
end;

procedure Menu(var IAData:MainGameData);
var
	choice:String;
	exitRequested:Boolean;
begin
	//Show a menu here, but for debug purposes, we'll say it's a New Game!
	//map adjustment can be made in "Custom game" option
	exitRequested:=false;
	repeat
		NewGame(IAData);
		exitRequested:=true;
	until exitRequested;
end;

procedure DrawMarker(Colour:Color;x:Integer;y:Single;radian:Single;startPoint,Length:Integer;facing:Direction);
var
	endPoint:Integer;
begin
	endPoint:=Length+startPoint;
	case facing of
		Left: DrawLine(Colour,x-startPoint*cos(radian),y-startPoint*sin(radian),x-endPoint*cos(radian),y-endPoint*sin(radian));
		Right: DrawLine(Colour,x+startPoint*cos(radian),y-startPoint*sin(radian),x+endPoint*cos(radian),y-endPoint*sin(radian));
	end;
	
end;

procedure DrawPlayers(IAData:MainGameData);
var
	i:Integer;
	rad,y,x,textPos:Single;
	vehicleBMP,gunBMP:Bitmap;
	name:String;
begin
	for i:=Low(IAData.Player)  to High(IAData.Player) do
	begin
		if (IAData.Player[i].Exists) then
		begin
			x:=IAData.Player[i].pos;
			vehicleBMP:=IAData.SPG[Ord(IAData.Player[i].SPG)].Sprite.HullBitmap;
			y:=GroundPosition(Round(x),IAData);
			
			DrawBitmap(vehicleBMP,x,y);
			FillCircle(ColorBlack,x,y,3);
			name:=IAData.Player[i].Name;
			textPos:=x-Length(name)*3.5; //Length one one SwinGame character is 7 pixels
			DrawText(Name,ColorBlack,textPos,y-30);
			case IAData.Player[i].Facing of
				Left: 
					begin
					FillCircle(ColorRed,x-4,y,3);
					end;
				Right: 
					begin
					FillCircle(ColorRed,x+4,y,3); 
					end;
			end;
		end;
	end;
end;

procedure DrawHUD(IAData:MainGameData);
var
	vehicleName:String;
	playerVehicle:SPGType;
	hudString,gunString,angleString,speedString,powerString,nameString:String;
	gunPosition:Single;
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	playerVehicle:=IAData.Player[i].SPG;
	vehicleName:=IAData.SPG[Ord(playerVehicle)].Name;
	Str(IAData.Player[i].GunAngle * 360 / (2 * pi):0:2,angleString);
	Str(Abs(IAData.Player[i].vel):0:2,speedString);
	Str(Abs(IAData.Player[i].ProjectilePower):0:2,powerString);
	nameString:=Concat('Current Player: ',IAData.Player[i].Name);
	hudString:=Concat('Vehicle: ',vehicleName);
	gunString:=Concat('Angle:   ',angleString,' Degrees');
	speedString:=Concat('Speed:   ',speedString,' px/l');
	powerString:=Concat('Power:   ',powerString,' px/l');
	
	DrawText(nameString,ColorBlack,50,35);
	DrawText(hudString,ColorBlack,50,50);
	DrawText(gunString,ColorBlack,50,60);
	DrawText(speedString,ColorBlack,50,70);
	DrawText(powerString,ColorBlack,50,80);
	DrawInterface();
end;

procedure DrawProjectiles(IAData:MainGameData);
var
	i,j,k:Integer;
begin
	i:=IAData.CurrentPlayer;
	
	for j:=0 to High(IAData.Projectile) do
	begin
		if (IAData.Projectile[j].Exists) then
		begin
			FillCircle(ColorRed,IAData.Projectile[j].x,SCREEN_HEIGHT-IAData.Projectile[j].y,PROJECTILE_RADIUS);
			
			//draw tracers and simulate their fade
		end;
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			//WriteLn('Tracer ',k,' current state: ',IAData.Projectile[j].Tracer[k].Exists, ', Fade timer:',IAData.Projectile[j].Tracer[k].FadeTime);
			if (IAData.Projectile[j].Tracer[k].Exists) then
			begin
				FillCircle(IAData.Projectile[j].Tracer[k].Colour,IAData.Projectile[j].Tracer[k].x,SCREEN_HEIGHT-IAData.Projectile[j].Tracer[k].y,PROJECTILE_RADIUS);
			end;
		end;
	end;
end;

procedure DrawAngleMarkers(IAData:MainGameData);
var
	i,x:Integer;
	y,radian,radDepression,radElevation,radVechicle:Single;
begin
	i:=IAData.CurrentPlayer;
	x:=Round(IAData.Player[i].pos);
	y:=GroundPosition(x,IAData);
	radVechicle:=IAData.Player[i].VehicleAngle;
	radian:=IAData.Player[i].GunAngle+radVechicle;
	radElevation:=Rad(IAData.SPG[Ord(IAData.Player[i].SPG)].GunElevation)+radVechicle;
	radDepression:=-Rad(IAData.SPG[Ord(IAData.Player[i].SPG)].GunDepression)+radVechicle;
	
	DrawMarker(ColorBlack,x,y,radian,10,20,IAData.Player[i].Facing);
	DrawMarker(ColorRed,x,y,radElevation,10,20,IAData.Player[i].Facing);
	DrawMarker(ColorRed,x,y,radDepression,10,20,IAData.Player[i].Facing);
end;

procedure DrawHealthBars(IAData:MainGameData);
var
	i,x,y,w,h,fill:Integer;
begin
	w:=40;
	h:=4;
	for i:=Low(IAData.Player) to High(IAData.Player) do
	begin
		if (IAData.Player[i].Exists) then
		begin
			fill:=Round( (w-2) * (IAData.Player[i].HP / IAData.SPG[Ord(IAData.Player[i].SPG)].MaxHP));
			x:=Round(IAData.Player[i].pos - w / 2);
			y:=GroundPosition(Round(IAData.Player[i].pos),IAData)+10;
			FillRectangle(ColorWhite,x,y,w,h);
			FillRectangle(ColorRed,x+1,y+1,w-2,h/2);
			FillRectangle(ColorGreen,x+1,y+1,fill,h/2);
		end;
	end;
end;

procedure DrawDamageText(IAData:MainGameData);
var
	i:Integer;
	Alpha,x,y:Single;
begin
	for i:=0 to High(IAData.DamageText) do
	begin
		if (IAData.DamageText[i].Exists) then
		begin
		Alpha:=IAData.DamageText[i].FadeTime / DAMAGE_FADE_TIME;
		x:=IAData.DamageText[i].x;
		y:=IAData.DamageText[i].y;
		DrawText(IAData.DamageText[i].Value,RGBAFloatColor(0,0,0,Alpha),x,y);
		end;
	end;
end;

procedure DrawGame(IAData:MainGameData);
begin
	DrawMap(IAData.GroundMap);
	DrawPlayers(IAData);
	DrawHealthBars(IAData);
	DrawProjectiles(IAData);
	DrawAngleMarkers(IAData);
	DrawDamageText(IAData);
	DrawHUD(IAData);
end;

procedure InitialiseCombat(var IAData:MainGameData);
var
	i,j,k:Integer;
	newMap: Map;
begin
	//MapType will generate terrain, hence, GenerateTerrain and similar functions need reworking
	//newMap:=RandomMapType()
	//ClearTerrain(IAData.GroundMap);
	GenerateTerrain(IAData.GroundMap,round(SCREEN_HEIGHT / 5),fRandBtw(0.7, 1.6), Linear);
	IAData.currentPlayer:=RandomPlayer(IAData);
	IAData.ActiveShellIndex:=0;
	ShowPanel(IAData.ShellPanel);
	for i:=0 to High(IAData.Player) do
	begin
		IAData.Player[i].Pos:=Rnd(SCREEN_WIDTH-10)+5;
		IAData.Player[i].Exists:=true;
		IAData.Player[i].GunAngle:=0;
		IAData.Player[i].Facing:=Direction(Rnd(2));
		IAData.Player[i].HP:=IAData.SPG[Ord(IAData.Player[i].SPG)].MaxHP;
	end;
	
	for j:=0 to High(IAData.Projectile) do
	begin
		IAData.Projectile[j].Exists:=false;
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			IAData.Projectile[j].Tracer[k].Exists:=false;
		end;
	end;
	
	for j:=0 to High(IAData.DamageText) do
	begin
		IAData.DamageText[j].Exists:=false;
	end;
end;

function AngleOfSlope(IAData:MainGameData):Single; //Returns Angle in Rad
var
	newHeight, currentHeight, heightDiff, angle:Single;
	pos,distanceTo:Integer;
	i:Integer;
	facing:Direction;
begin
	i:=IAData.CurrentPlayer;
	pos:=Round(IAData.Player[i].pos);
	facing:=IAData.Player[i].Facing;
	currentHeight:=IAData.GroundMap[pos];
	newHeight:=currentHeight;
	if (facing = Right) then newHeight:=IAData.GroundMap[pos+VEHICLE_SLOPE_RADIUS];
	if (facing = Left) then newHeight:=IAData.GroundMap[pos-VEHICLE_SLOPE_RADIUS];
	heightDiff:=newHeight-currentHeight;
	
	if not (newHeight=currentHeight) then
	begin
		angle:=arctan(heightDiff / VEHICLE_SLOPE_RADIUS);
		result:=angle;
	end else begin
		result:=0;
	end;
end;

function SlopeMultiplier(IAData:MainGameData):Single;
begin
	result:=1-(AngleOfSlope(IAData) * VEHICLE_SLOPE_MULTIPLIER);
end;

procedure SimulateVehicleMovement(var IAData:MainGameData);
var
	acc,slope:Single;
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	acc:=IAData.Player[i].acc;
	slope:=SlopeMultiplier(IAData);
	IAData.Player[i].vel+=acc;
	IAData.Player[i].vel*=slope;
	IAData.Player[i].vel*=VEHICLE_FRICTION;
	if (IAData.Player[i].BrakesOn) then IAData.Player[i].vel*=VEHICLE_BRAKE;
	if (IAData.Player[i].vel > VEHICLE_MAXSPEED) then IAData.Player[i].vel:=VEHICLE_MAXSPEED; 
	if (IAData.Player[i].vel < -VEHICLE_MAXSPEED) then IAData.Player[i].vel:=-VEHICLE_MAXSPEED; 
	IAData.Player[i].pos+=(IAData.Player[i].vel);
end;

procedure UpdatePlayerFacing(var IAData:MainGameData);
var
	acc:Single;
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	acc:= IAData.Player[i].acc;
	if (acc > 0) then IAData.Player[i].Facing:=Right;
	if (acc < 0) then IAData.Player[i].Facing:=Left;
end;

procedure UpdateVehicleAngle(var IAData:MainGameData);
var
	i:Integer;
	slopeAngle:Single;
begin
	i:=IAData.CurrentPlayer;
	slopeAngle:=AngleOfSlope(IAData);
	
	IAData.Player[i].VehicleAngle:=slopeAngle;
end;

procedure CyclePlayers(var IAData:MainGameData);
var
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	repeat
	i+=1;
	until IAData.Player[i].Exists;
	
	if (i>High(IAData.Player)) then i:=Low(IAData.Player);
	IAData.CurrentPlayer:=i;
end;

procedure FireProjectile(var IAData:MainGameData);
var
	i,j,k:Integer;
	emptySlotFound:Boolean;
	effectivePower,angle:Single;
begin
	i:=IAData.CurrentPlayer;
	emptySlotFound:=false;
	effectivePower:=IAData.Player[i].ProjectilePower * POWER_TO_PXL;
	angle:=IAData.Player[i].GunAngle+IAData.Player[i].VehicleAngle;
	//WriteLn('Firing projectile Now! with power: ',IAData.Player[i].ProjectilePower:0:2);
	
	j:=-1;
	repeat
	j+=1;
	if not (IAData.Projectile[j].Exists) then 
	begin
		emptySlotFound:=true;
		IAData.Projectile[j].Exists:=true;
		IAData.Projectile[j].Owner:=i;
		IAData.Projectile[j].Shell:=ShellType(IAData.ActiveShellIndex);
		IAData.Projectile[j].x:=IAData.Player[i].pos;
		IAData.Projectile[j].y:=IAData.GroundMap[Round(IAData.Player[i].pos)];
		IAData.Projectile[j].dx:=effectivePower * cos(angle);
		IAData.Projectile[j].dy:=effectivePower * sin(angle);
		if (IAData.Player[i].Facing=Left) then IAData.Projectile[j].dx*=-1;
		
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			IAData.Projectile[j].Tracer[k].Exists:=False;
			IAData.Projectile[j].Tracer[k].FadeTime:=0;
		end;
	end;
	if (j=High(IAData.Projectile)) then emptySlotFound:=true;
	until emptySlotFound;
	if (j=-1) then WriteLn('Maximum number of projectiles reached!');
	IAData.Player[i].ProjectilePower:=0;
	CyclePlayers(IAData);
end;

procedure DestroyGround(var IAData:MainGameData;pos:Single;Radius:Integer);
var
	i,x:Integer;
begin
	for i:=-Radius to Radius do
	begin
		x:=Round(pos)+i;
		if (x>0) and (x<High(IAData.GroundMap)) then
		begin
			IAData.GroundMap[x]-=(Sqrt(Sqr(Radius)-Sqr(i))) / 7;
			IAData.GroundMap[x]-=(Radius * cos( (i * 3.14) / (1.1 * Radius)) + 0.9 * Radius + rnd(3)) / 8;
		end;
	end;
end;

procedure CreateDamageText(var IAData:MainGameData; effectiveDamage:String;hitPlayer:Integer);
var
	j,k:Integer;
	objectCreated:Boolean;
begin
	objectCreated:=false;
	
	j:=-1;
	repeat
		j+=1;
		if not (IAData.DamageText[j].Exists) then
		begin
			IAData.DamageText[j].Exists:=true;
			IAData.DamageText[j].Value:=effectiveDamage;
			IAData.DamageText[j].x:=Round(IAData.Player[hitPlayer].pos);
			IAData.DamageText[j].y:=GroundPosition(Round(IAData.DamageText[j].x),IAData);
			IAData.DamageText[j].dy:=DAMAGE_TEXT_SPEED;
			IAData.DamageText[j].FadeTime:=DAMAGE_FADE_TIME;
			objectCreated:=true;
		end;
		if (j=High(IAData.DamageText)) then objectCreated:=true;
	until objectCreated;
end;

procedure DamagePlayers(var IAData:MainGameData; projectile:ProjectileData);
var
	i,j,xMin,xMax,radius,pos,distance:Integer;
	baseDamage,effectiveDamage:Integer;
	damageMultiplier:Single;
	damageString:String;
begin
	i:=projectile.Owner;
	pos:=Round(projectile.x);
	baseDamage:=IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(projectile.Shell)].Damage;
	radius:=IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(projectile.Shell)].BlastRadius;
	xMin:=pos - radius;
	xMax:=pos + radius;
	if (xMin<0) then xMin:=0;
	if (xMax>High(IAData.GroundMap)) then xMax:=High(IAData.GroundMap);
	
	
	for j:=0 to High(IAData.Player) do
	begin
		if (IAData.Player[j].Exists) then
		begin
			if (IAData.Player[j].pos > xMin) and  (IAData.Player[j].pos < xMax) then
			begin
				distance:=abs(Round(IAData.Player[j].pos - pos));
				damageMultiplier:=1-(Sqr(distance) / Sqr(radius));
				effectiveDamage:=Round(damageMultiplier * baseDamage);
				//WriteLn('Distance: ',distance,', Multiplier: ',damageMultiplier:0:2, 'Base Damage: ',baseDamage,' effectiveDamage: ',effectiveDamage, ' Player: ',j);
				//WriteLn('Position pos: ',projectile.x:0:2,' xMin: ',xMin,' xMax: ',xMax,' Playerpos: ',IAData.Player[j].pos:0:2);
				IAData.Player[j].HP-=effectiveDamage;
				str(effectiveDamage,damageString);
				if (damageMultiplier>0.97) then damageString:=Concat(damageString,'!!');
				CreateDamageText(IAData,damageString,j);
			end;
		end;
	end;
end;

procedure UpdateGun(var IAData:MainGameData);
var
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	if (IAData.Player[i].Exists) then
		if (IAData.Player[i].FireProjectile) then FireProjectile(IAData);
end;

procedure SimulateProjectiles(var IAData:MainGameData);
var
	i,j,k,explRadius:Integer;
	Alpha,velocityLoss:Single;
	tracerCreated:Boolean;
begin
	i:=IAData.CurrentPlayer;
	
	for j:=0 to High(IAData.Projectile) do
	begin
		if (IAData.Projectile[j].Exists) then
		begin
			IAData.Projectile[j].x+=IAData.Projectile[j].dx;
			IAData.Projectile[j].y+=IAData.Projectile[j].dy;
			
			velocityLoss:=1-IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(IAData.Projectile[j].Shell)].VelocityBleed;
			velocityLoss:=1-(velocityLoss*VELOCITY_LOSS_MULTIPLIER);
			//WriteLn('Velocity Bleed: ', velocityLoss:0:4);
			IAData.Projectile[j].dx*=velocityLoss;
			IAData.Projectile[j].dy-=GRAVITY_ACC;  
			
			if(IAData.Projectile[j].x>High(IAData.GroundMap)) then IAData.Projectile[j].Exists:=false;
			if(IAData.Projectile[j].x<0) then IAData.Projectile[j].Exists:=false;
			if(IAData.Projectile[j].y<0) then IAData.Projectile[j].Exists:=false;
			k:=0;
			tracerCreated:=false;
			repeat
				if not (IAData.Projectile[j].Tracer[k].Exists) then
				begin
					IAData.Projectile[j].Tracer[k].FadeTime:=TRACER_FADE_TIME;
					IAData.Projectile[j].Tracer[k].Exists:=true;
					IAData.Projectile[j].Tracer[k].Colour:=ColorRed;
					IAData.Projectile[j].Tracer[k].x:=IAData.Projectile[j].x;
					IAData.Projectile[j].Tracer[k].y:=IAData.Projectile[j].y;
					tracerCreated:=true;
				end;
				if (k>=High(IAData.Projectile[j].Tracer)) then tracerCreated:=true;
				k+=1;
			until tracerCreated;
			
			if(IAData.Projectile[j].y<IAData.GroundMap[Round(IAData.Projectile[j].x)]) then
			begin
				explRadius:=IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(IAData.Projectile[j].Shell)].BlastRadius;
				//WriteLn('Explosion Radius: ',explRadius);
				DestroyGround(IAData,IAData.Projectile[j].x,explRadius);
				DamagePlayers(IAData,IAData.Projectile[j]);
				IAData.Projectile[j].Exists:=false;
			end;
		end;
		
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			if (IAData.Projectile[j].Tracer[k].Exists) then
			begin
				IAData.Projectile[j].Tracer[k].FadeTime-=1;
				if (IAData.Projectile[j].Tracer[k].FadeTime<=0) then
				begin
					IAData.Projectile[j].Tracer[k].FadeTime:=0;
					IAData.Projectile[j].Tracer[k].Exists:=false;
				end else begin //Fade Time >0
					Alpha:= (IAData.Projectile[j].Tracer[k].FadeTime / TRACER_FADE_TIME);
					IAData.Projectile[j].Tracer[k].Colour:=RGBAFloatColor(1,0,0,Alpha);
					//WriteLn('Alpha of Tracer ',k,': ',Alpha:0:2);
				end;
			end;
		end;
		
	end;
end;

procedure SimulateDamageText(var IAData:MainGameData);
var
	i:Integer;
begin
	for i:=0 to High(IAData.DamageText) do
	begin
		if (IAData.DamageText[i].Exists) then
		begin
			IAData.DamageText[i].y-=IAData.DamageText[i].dy;
			IAData.DamageText[i].FadeTime-=1;
		end;
		
		if (IAData.DamageText[i].FadeTime<=0) then
		begin
			IAData.DamageText[i].Exists:=false;
		end;
		
	end;
end;

procedure UpdateExistences(var IAData:MainGameData);
var
	i:Integer;
begin
	for i:=0 to High(IAData.Player) do
	begin
		if (IAData.Player[i].Exists) then
		begin
			if (IAData.Player[i].HP <= 0) then
			begin
				IAData.Player[i].Exists:=false;
				CyclePlayers(IAData);
				//drawExplosion
			end;
		end;
	end;
end;

procedure UpdateGame(var IAData:MainGameData);
var
	j:Integer;
begin
	UpdateInterface();
	UpdatePlayerFacing(IAData);
	SimulateVehicleMovement(IAData);
	UpdateVehicleAngle(IAData);
	UpdateGun(IAData);
	SimulateProjectiles(IAData);
	SimulateDamageText(IAData);
	UpdateExistences(IAData);
end;

procedure UpdateSelectedShell(var IAData:MainGameData);
begin
	case RegionClickedID() of
		'ShellButton1' : IAData.ActiveShellIndex:=0;
		'ShellButton2' : IAData.ActiveShellIndex:=1;
		'ShellButton3' : IAData.ActiveShellIndex:=2;
		'ShellButton4' : IAData.ActiveShellIndex:=3;
		'ShellButton5' : IAData.ActiveShellIndex:=4;
		'ShellButton6' : IAData.ActiveShellIndex:=5;
		'ShellButton7' : IAData.ActiveShellIndex:=6;
	end;
end;

procedure HandleInput(var IAData:MainGameData);
var
	radIncrement,radElevation,radDepression,maxPower:Single;
	i:Integer;
begin
	ProcessEvents();
	UpdateInterface();
	UpdateSelectedShell(IAData);
	i:=IAData.CurrentPlayer;
	IAData.Player[i].acc:=0;
	IAData.Player[i].BrakesOn:=true;
	IAData.Player[i].FireProjectile:=false;
	radIncrement:=DEGREE_INCREMENT * 2 * pi / 360;
	radElevation:=Rad(IAData.SPG[Ord(IAData.Player[i].SPG)].GunElevation);
	radDepression:=-Rad(IAData.SPG[Ord(IAData.Player[i].SPG)].GunDepression);
	maxPower:=IAData.SPG[Ord(IAData.Player[i].SPG)].MaxGunVelocity;
	
	if (KeyDown(LeftKey)) then IAData.Player[i].acc := -VEHICLE_ACC;
	if (KeyDown(RightKey)) then IAData.Player[i].acc := VEHICLE_ACC;
	if (IAData.Player[i].acc<>0) then IAData.Player[i].BrakesOn := false;
	if (KeyDown(ShiftKey)) then IAData.Player[i].BrakesOn := false;
	if (KeyDown(RightShiftKey)) then IAData.Player[i].BrakesOn := false;
	if (KeyDown(SpaceKey)) then IAData.Player[i].ProjectilePower += POWER_INCREMENT;
	if (KeyReleased(SpaceKey)) then IAData.Player[i].FireProjectile := true;
	   
	if (KeyDown(UpKey)) then IAData.Player[i].GunAngle += radIncrement;
	if (KeyDown(DownKey)) then IAData.Player[i].GunAngle -= radIncrement;
	
	if (IAData.Player[i].pos<0) then IAData.Player[i].pos:=0;
	if (IAData.Player[i].pos>High(IAData.GroundMap)) then IAData.Player[i].pos:=High(IAData.GroundMap);
	if (IAData.Player[i].ProjectilePower > maxPower) then IAData.Player[i].ProjectilePower:=maxPower;
	if (IAData.Player[i].GunAngle > radElevation) then IAData.Player[i].GunAngle:=radElevation;
	if (IAData.Player[i].GunAngle < radDepression) then IAData.Player[i].GunAngle:=radDepression;
	
end;

procedure Battle(var IAData:MainGameData);
var
	battleEnded:Boolean;
begin
	battleEnded:=false;
	repeat
		HandleInput(IAData);
		UpdateGame(IAData);
		DrawGame(IAData);
		RefreshScreen(TARGET_FPS);
		
		if (WindowCloseRequested()) then battleEnded:=true;
	until battleEnded;
end;

procedure SetupPanels(var IAData:MainGameData);
begin
	IAData.ShellPanel:=LoadPanel('shellGroup.txt');
end;

procedure Main();
var
	IAData:MainGameData;
begin
	OpenAudio();
	InitialiseData(IAData,PathToResource(ARTY_DATA_FILE));
	SetupPanels(IAData);
	LoadResources(IAData);
	OpenGraphicsWindow('Artillery',SCREEN_WIDTH,SCREEN_HEIGHT);
	
	repeat
		Menu(IAData);
		InitialiseCombat(IAData);
		Battle(IAData);
	until WindowCloseRequested();
	ReleaseAllResources();
end;

begin
	Main();
end.