program ArtilleryV;
uses {imagination} Swingame, sgTypes, SysUtils;
{
	Game in which players learn how to best shoot one another.
	Created by Jimmy Trac, ID:1016 24 964
}
Const
	VERSION='V v. 12-10';
	MOTD='We''re done!';
	SCREEN_HEIGHT=500;
	SCREEN_WIDTH=SCREEN_HEIGHT * 16 div 9;
	MAP_SIZE=2000;					//size of terrain. Actual size will be given by a multiple of 2 higher than this number
	CAMERA_SPEED=1;
	CAMERA_EASE_SPEED=10;
	TARGET_FPS=60;
	GRAVITY_ACC=0.3;
	VEHICLE_MAXSPEED=0.95;			//maximum speed units/loop
	VEHICLE_ACC=0.15;				//0.5 px/(l^2)
	VEHICLE_BRAKE=0.4;				//Acts as friction, 0~1 . Brakes always kept on to limit top speed of vehicles.
	VEHICLE_FRICTION=0.8;			//When brake is off
	VEHICLE_SLOPE_RADIUS=5;			//Radius to scan to determine the slope
	VEHICLE_SIZE=5;					//Size in pixels to scan and determine vehicle position
	VEHICLE_SLOPE_MULTIPLIER=0.5;	//Determines how strongly slopes affect acceleration
	VEHICLE_SCALE_FACTOR=0.02;		//Scale bitmap
	DEGREE_INCREMENT=0.5;			//Increment per loop, in degrees
	POWER_INCREMENT=5;				//Increment per loop
	POWER_TO_PXL=0.026;
	ARTY_DATA_FILE='data.ia';
	PROJECTILE_RADIUS=2;
	VELOCITY_LOSS_MULTIPLIER=0.7;		//How much to reduce loss by and increase distance
	MAX_ENTITIES=30;
	MAX_PROJECTILES=30;
	MAX_TRACERS=10;
	TRACER_FADE_TIME=MAX_TRACERS;		//Number of loops before tracer disappears
	DAMAGE_FADE_TIME=100;
	DAMAGE_TEXT_SPEED=1;				//movement speed for text, vertically.
	EXPLOSION_FADE_TIME=30;
	TEXT_FADE_IN_TIME=150;
	TEXT_FADE_OUT_TIME=50;
	GIRL_FADE_TIME=50;
	GIRL_POSITION_OFFSET=10;
	GIRL_HEIGHT_OFFSET=-20;
	GIRL_MOVE_SPEED=3;
	POWER_BAR_HEIGHT=29;
	POWER_BAR_WIDTH=375;
	TRAVERSE_BAR_HEIGHT=11;
	TRAVERSE_BAR_WIDTH=375;
	WIND_ANGLE_LIMIT=40;			// degrees from the horizontal, max of 90.
	WIND_MIN_MAGNITUDE=0;
	WIND_MAX_MAGNITUDE=0.35;
	WIND_INTERVAL_MAX=4;			//Number of players cycled before wind changes
	WIND_INTERVAL_MIN=2;			//multiplied by number of players
	WIND_START_MIN=4;				//Number of turns before wind starts
	WIND_START_MAX=8;
	WIND_MARKER_Y=50;				//Y-Position of the Wind Marker.
	WIND_MARKER_SIZE=20;			//Size of wind marker
	WIND_MARKER_RAD_SIZE=120;		//Number of degrees to offset the sub-points by
	WIND_MARKER_MOVE_SPEED=10;
	EFFECT_INTERVAL_MIN=2;
	EFFECT_INTERVAL_MAX=4;
	EFFECT_DURATION_MIN=3;
	EFFECT_DURATION_MAX=10;
	EFFECT_TEXT_Y=100;
	EFFECT_ALPHA_MIN=0.1;
	EFFECT_MIRROR_RADIUS=3;
	EFFECT_MIRROR_ALPHA_MIN=0.5;
	EFFECT_BASE_POWER_GB=0.8;		//For GB values of RGB
	EFFECT_BASE_RADIUS=15;
	EFFECT_BASE_MULTIPLY_RB=0.8;
	EFFECT_POWER_MIN=1.1;
	EFFECT_POWER_MAX=3;
	EFFECT_MULTIPLY_MIN=1;
	EFFECT_MULTIPLY_MAX=4;
	EFFECT_MULTIPLY_MIN_DEVIATION=0.6;
	EFFECT_MULTIPLY_MAX_DEVIATION=1.4;
	EFFECT_DEGREE_INCREMENT=2;
	EFFECT_HEIGHT=150;
	MAX_EFFECTS_CREATED=4;				//maximum effects to be created per constructor
	MINIMAP_WIDTH=250;
	MINIMAP_HEIGHT=20;
	MINIMAP_X=300;		//from right edge
	MINIMAP_Y=40;		//from top

	Type
		HeightMap = Array of Single;
		SPGType = (GWTiger, CGC, OBJ);
		ShellType = (HE, HEAT, HESH, APHE);
		TeamType = (None, Red, Green, Blue, White, Black); //for MP
		TerrainType = (Hills, Plains, Mountains, Wasteland, Barren, Twilight);
		TerrainReduction = (Linear, Exponential);
		Direction = (Left, Right);
		EffectType = (PowerUp, Multiply, Mirror);
		CameraTracking = (Player, Projectile, Minimap);
	//
	
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
		dx,dy,x,y:Single;
	end;
	
	ProjectileData = Record
		Exists:Boolean;
		Owner:Integer;			//Player that fired the projectile
		Shell:ShellType;		//To determine damage
		Colour:Color;			//"Color" = Type, "Colour" = everything else
		IsInEffect:Boolean;		//Is the projectile currently in an effect?
		x,y:Single;				//Originally used swingame, was useless.
		dx,dy:Single;			//velocity
		PowerUp:Single;
		Tracer: Array[0..MAX_TRACERS-1] of TracerData;
	end;
	
	DamageTextData = Record
		Exists:Boolean;
		Value:String;
		x,y,dy:Single;
		FadeTime:Integer;
	end;
	
	GirlData = Record
		Exists:Boolean;
		Side:Direction;
		Looks:Bitmap;
		x,y,dx:Single;
		FadeTime:Integer;
	end;
	
	ExplosionData = Record
		Exists:Boolean;
		Radius:Integer;
		x,y:Single;
		FadeTime:Integer;
	end;
	
	EffectData = Record
		Exists:Boolean;
		EffectOf:EffectType;
		Colour:Color;
		Alpha:Single;
		AlphaAngle:Single;
		PowerUp:Single;		//multiplier
		Multiply:Integer;
		Radius:Integer;
		x:Single;
		FadeCounter:Integer;
	end;
	
	PlayerData = Record
		Name: String;
		SPG:SPGType;
		Sprite:SPGSprite;
		Team:TeamType;
		HP:Integer;
		Exists:Boolean;
		AmmunitionRack: Array[0..Integer(High(ShellType))] of Ammunition;
		TraverseLeft:Single;
		Credits:Integer;
		Acc,Vel,Pos:Single;				//Acceleration, Velocity, X-Position
		Facing:Direction;				//Current vehicle facing
		GunAngle:Single;				//Current gun Angle; In radians
		VehicleAngle:Single;			//Angle of player's Vehicle on Terrain
		BrakesOn:Boolean;				//Whether or not brakes are on. Normally on.
		ProjectilePower:Single;			//for shooting
		PreviousProjectilePower:Single;			//for shooting
		FireProjectile:Boolean;
		ActiveShellIndex:Integer;
	end;
	
	WindData = Record
		Exists: Boolean;
		Magnitude: Single;
		Direction, TargetDirection, DrawDirection: Single;
		Facing: Direction;
	end;
	
	ProgressBarData = Record
		Full,Empty:Bitmap;
	end;
	
	CameraData = Record
		x,y:Single;
		Tracking:CameraTracking;
	end;
	
	MainGameData = Record
		Camera:CameraData;
		Player: Array of PlayerData;
		SPG: Array of SPGData;
		Projectile: Array[0..MAX_PROJECTILES-1] of ProjectileData;
		DamageText: Array[0..MAX_ENTITIES-1] of DamageTextData;
		Explosion: Array[0..MAX_ENTITIES-1] of ExplosionData;
		ShellColour: Array[0..Integer(High(ShellType))] of Color; //Array index is the same as shell type.
		Girl: Array[0..MAX_ENTITIES-1] of GirlData;
		Effect: Array[0..MAX_ENTITIES-1] of EffectData;
		Wind: WindData;
		WindCounter: Integer;
		EffectCounter: Integer;
		BGColour:Color;
		ShellPanel,ShellDesc:Panel;
		GroundMap: HeightMap;
		CurrentPlayer:Integer;
		ShowingShellDesc:Boolean;
		PowerBar:ProgressBarData;
		TraverseBar:ProgressBarData;
		MenuExitRequested:Boolean;
		ExitRequested:Boolean;
		NewGameRequested:Boolean;
	end;

procedure DrawMap(Colour:Color;groundMap:HeightMap);
//Draws the map given the Colour and the map data.
var
	i:Integer;
begin
	for i:=0 to High(groundMap) do
	begin
		DrawLine(Colour,i,SCREEN_HEIGHT-groundMap[i],i,SCREEN_HEIGHT);
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

procedure ClearTerrain(var groundMap:HeightMap);
//Clears the given map to 0's
var
	i:Integer;
begin
	for i:=Low(groundMap) to High(groundMap) do
	begin
		groundMap[i]:=0;
	end;
end;

function Rad(degr:Single):Single;
begin
	result:=degr * 2 * pi / 360;
end;

function Deg(Radi:Single):Single;
begin
	result:=Radi * 360 / ( 2 * pi);
end;


function buttonClicked(buttonX, buttonY: Single; buttonWidth, buttonHeight: Integer): Boolean;
//Basic button function. Returns true if the mouse is within the bounds specified.
var
	x, y: Single;
	buttonRightLimit, buttonBottomLimit: Single;
begin
	x := MouseX();
	y := MouseY();
	buttonRightLimit := buttonX + buttonWidth;
	buttonBottomLimit := buttonY + buttonHeight;
	result := false;
	
	if MouseClicked(LeftButton) then
	begin
		if (buttonX<=x) and (x<=buttonRightLimit) and (buttonY<=y) and (y<=buttonBottomLimit) then
		begin
			result := true;
		end;
	end;
end;

procedure DrawTextCentre(Colour:Color;message:String;x,y:Single);
//Draws text centred around the y-position. 
//This is based on the Swingame type having a width of 7 pixels per character.
var
	textPos:Single;
begin
	textPos:=x-Length(message)*3.5; 
	DrawText(Message,Colour,textPos,y);
end;

procedure DrawTextCentreOnScreen(Colour:Color;message:String;x,y:Single);
//Draws text centred around the y-position on screen
//This is based on the Swingame type having a width of 7 pixels per character.
var
	textPos:Single;
begin
	textPos:=x-Length(message)*3.5; 
	DrawTextOnScreen(Message,Colour,textPos,y);
end;

procedure GenerateTerrain(var groundMap:HeightMap; displacement:Integer; roughness:Double; reductionType:TerrainReduction);
//Generates terrain and outputs to the map array.
//Displacement refers to the maximum displacement possible.
//roughness determines how "smooth" the terrain looks. Higher roughness means higher roughness reduction, producing smoother terrain.
//Terrain reduction type can be linear or exponential. One looks smoother than the other.
var
	i,j,currentIndex,segments,segmentLength,terrainWidth,exponent:Integer;
	reduction:Double;
begin
	reduction:= 1 / fPower(2,roughness);
	
	exponent:=powerCeil(MAP_SIZE,2);
	terrainWidth:=power(2,exponent);
	setLength(groundMap, terrainWidth+1);
	
	groundMap[0]:=SCREEN_HEIGHT/3 + randDisplacement(displacement);
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

function between(x,x1,x2:Single):Boolean;
begin
	result:=false;
	if (x1 < x2) then 
	begin
		if (x1 < x) and (x < x2) then result:=true;
	end else begin
		if (x2 < x) and (x < x1) then result:=true;
	end;
end;

function GroundPosition(x:Integer;IAData:MainGameData):Integer;
//Changes the y-axis from being at the top (y=0) so that it starts from the bottom.
begin
	result:=Round(SCREEN_HEIGHT-IAData.GroundMap[x]);
end;

function ReadBooleanLine(var dataFile:TextFile):Boolean;
//Reads a boolean line from the supplied file. 1 = true, else, false.
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
//Initialises all game data and plugs them into the data types described.
var
	dataFile: TextFile;
	arty,i:Integer;
begin
	//WriteLn('Loading from ',dataLocation);
	AssignFile(dataFile,dataLocation);
	reset(dataFile);
	//WriteLn('Data File file opened');
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
		//WriteLn('passed main, entering into loop for shells for ARTY: ',arty);
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
			//WriteLn('passed shell loop for ShellType: ',i);
		end;
		//WriteLn('passed shell loops.');
		ReadLn(dataFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.HullFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.BarrelFile);
		ReadLn(dataFile,IAData.SPG[arty].Sprite.GirlFile);
		ReadLn(dataFile,IAData.SPG[arty].TraverseLimit);
		ReadLn(dataFile,IAData.SPG[arty].MaxNumberShells);
		ReadLn(dataFile);
		//WriteLn('end of loop.');
	end;
end;

procedure LoadResources(var IAData:MainGameData);
//loads resources from files. Comes after InitialiseData as file locations are needed.
var
	i:Integer;
begin
	for i:=0 to Integer(High(SPGType)) do
	begin
	LoadBitmapNamed('shellsdesc.PNG','shellsdesc.PNG');
	
	LoadBitmapNamed(IAData.SPG[i].Sprite.HullFile,IAData.SPG[i].Sprite.HullFile);
	LoadBitmapNamed(IAData.SPG[i].Sprite.BarrelFile,IAData.SPG[i].Sprite.BarrelFile);
	LoadBitmapNamed(IAData.SPG[i].Sprite.GirlFile,IAData.SPG[i].Sprite.GirlFile);
	
	IAData.SPG[i].Sprite.HullBitmap:=BitmapNamed(IAData.SPG[i].Sprite.HullFile);
	IAData.SPG[i].Sprite.BarrelBitmap:=BitmapNamed(IAData.SPG[i].Sprite.BarrelFile);
	IAData.SPG[i].Sprite.GirlBitmap:=BitmapNamed(IAData.SPG[i].Sprite.GirlFile);
	
	IAData.SPG[i].Sprite.HullBitmap:=RotateScaleBitmap(IAData.SPG[i].Sprite.HullBitmap,0,VEHICLE_SCALE_FACTOR);
	end;
	
	//Load shell colours
	IAData.ShellColour[0]:=RGBAColor(245,110,10,255);
	IAData.ShellColour[1]:=RGBAColor(150,150,100,255);
	IAData.ShellColour[2]:=RGBAColor(145,250,180,255);
	IAData.ShellColour[3]:=RGBAColor(125,3,3,255);
	
end;

procedure SetShellTypes(IAData:MainGameData);
//Had some issues with returning radio button IDs, so this function forces
//the checkboxes to act as radio buttons by forcing them on or off.   
var                                                    
	i:Integer;                                         
begin                                                  
	i:=IAData.CurrentPlayer;                           

	CheckBoxSetState('ShellButton1', false);           
	CheckBoxSetState('ShellButton2', false);           
	CheckBoxSetState('ShellButton3', false);           
	CheckBoxSetState('ShellButton4', false);           
	CheckBoxSetState('InfoButton', IAData.ShowingShellDesc);           
	
	case IAData.Player[i].ActiveShellIndex of
		0: CheckBoxSetState('ShellButton1', true);
		1: CheckBoxSetState('ShellButton2', true);
		2: CheckBoxSetState('ShellButton3', true);
		3: CheckBoxSetState('ShellButton4', true);
	end;
	
end;

function RandomPlayer(IAData:MainGameData):Integer;
//Returns the index of a random player given the number of players
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

function HandleNumberInput():Integer;
//Used to select the number of players. 
var
	i:Integer;
begin
	result:=-1;
	for i:=48 to 57 do //0 to 9 Keycode
	begin
		if KeyDown(KeyCode(i)) then result:=i - 48;
	end;
end;

procedure SelectNumberPlayers(var IAData:MainGameData); //Will show a panel indicating number of players
//Used in menu to select the number of players.
var
	numberPlayers:Integer;
	exitRequested:Boolean;
	guiFont:Font;
begin
	exitRequested:=false;
	numberPlayers:=1;
	guiFont:=LoadFontNamed('guiFont','maven_pro_regular.ttf',12);
	StartReadingText(ColorBlack,20,guiFont,Round(SCREEN_WIDTH/2),150);
	repeat
		ProcessEvents();
		ClearScreen(ColorWhite);
		
		DrawTextCentre(ColorBlack,'Number of Players:',SCREEN_WIDTH/2,120);
		if not ReadingText() then begin numberPlayers:=StrToInt(EndReadingText()); exitRequested:=true; end;
		if WindowCloseRequested() then begin exitRequested:=true; IAData.ExitRequested:=true; end;
		RefreshScreen(TARGET_FPS);
	until exitRequested;
	SetLength(IAData.Player,numberPlayers);
end;

procedure ReadUserNames(var IAData:MainGameData);
var
	i:Integer;
	prompt,strPlayer:String;
	exitRequested:Boolean;
	guiFont:Font;
begin
	guiFont:=LoadFontNamed('guiFont','maven_pro_regular.ttf',12);
	for i:=0 to High(IAData.Player) do
	begin
		exitRequested:=false;
		StartReadingText(ColorBlack,20,guiFont,Round(SCREEN_WIDTH/2),150);
		repeat
			ProcessEvents();
			ClearScreen(ColorWhite);
			str(i+1,strPlayer);
			prompt:=Concat('Player ',strPlayer,'''s name');
			DrawTextCentre(ColorBlack,prompt,SCREEN_WIDTH/2,120);
			if not ReadingText() then begin IAData.Player[i].Name:=EndReadingText(); exitRequested:=true; end;
			RefreshScreen(TARGET_FPS);
		until exitRequested;
		//IAData.Player[i].Name:=ReadString('Name: ');
	end;
end;

procedure SelectPlayerVehicles(var IAData:MainGameData);
var
	i,j,k,offset:Integer;
	prompt:String;
	exitRequested:Boolean;
	Colour:Color;
begin
	Colour:=RGBAFloatColor(0.4,0.4,0.4,1);
	offset:=30;
	
	for i:=Low(IAData.Player) to High(IAData.Player) do
	begin
		IAData.Player[i].SPG:=SPGType(-1);
	end;
	for j:=Low(IAData.Player) to High(IAData.Player) do
	begin
		exitRequested:=false;
		//SelectVehicle(IAData.Player[i]);
		
		repeat
			ProcessEvents();
			ClearScreen(ColorWhite);
			prompt:=Concat(IAData.Player[j].Name,', pick your vehicle:');
			DrawTextCentre(ColorBlack,prompt,SCREEN_WIDTH/2,90);
			
			FillRectangle(RGBAFloatColor(0.94,0.94,0.94,0.7),0,110 + offset,SCREEN_WIDTH,160);
			DrawTextCentre(ColorBlack,'G.W. Tiger',Round(SCREEN_WIDTH/2),125 + offset);
			DrawTextCentre(Colour,'Long Gun Range, Low Damage, Powerful HEAT Shells',Round(SCREEN_WIDTH/2),140 + offset);
			DrawTextCentre(ColorBlack,'CGC',Round(SCREEN_WIDTH/2),175 + offset);
			DrawTextCentre(Colour,'High Damage, High Blast Radius, Low Range, Best HESH Shells',Round(SCREEN_WIDTH/2),190 + offset);
			DrawTextCentre(ColorBlack,'Object',Round(SCREEN_WIDTH/2),225 + offset);
			DrawTextCentre(Colour,'Very good Damage, Good Range, Low HP, Strong APHE Shells',Round(SCREEN_WIDTH/2),240 + offset);
			
			for k:=Low(IAData.Player) to j-1 do
			begin
				if (IAData.Player[k].SPG<>SPGType(-1)) then begin
					prompt:=Concat(IAData.Player[k].Name,' : ',IAData.SPG[Ord(IAData.Player[k].SPG)].Name);
				end else begin
					prompt:=Concat(IAData.Player[k].Name,': ');
				end;
				DrawTextCentre(ColorBlack,prompt,SCREEN_WIDTH/2,310+(k+1)*20);
			end;
			
			if buttonClicked(210,140,512,52) then begin IAData.Player[j].SPG:=SPGType(0); exitRequested:=true; end;
			if buttonClicked(210,190,512,52) then begin IAData.Player[j].SPG:=SPGType(1); exitRequested:=true; end;
			if buttonClicked(210,240,512,52) then begin IAData.Player[j].SPG:=SPGType(2); exitRequested:=true; end;
			
			if WindowCloseRequested() then begin exitRequested:=true; IAData.ExitRequested:=true; end;
			RefreshScreen(TARGET_FPS);
		until exitRequested;
		
	end;
end;

procedure NewGame(var IAData:MainGameData);
//Start a new game as requested from menu.
begin
	SelectNumberPlayers(IAData);
	ReadUserNames(IAData);
	SelectPlayerVehicles(IAData);
end;

procedure DrawMenu(IAData:MainGameData;MenuAlpha:Integer);
var
	middleOfScreen,i:Integer;
	alpha:Single;
	menuColour,mapColour:Color;
	menuStr:String;
begin
	ClearScreen(ColorWhite);
	middleOfScreen:=Round(SCREEN_WIDTH/2);
	alpha:=MenuAlpha / TEXT_FADE_IN_TIME;
	
	menuColour:=RGBAFloatColor(0,0,0,alpha);
	menuStr:=Concat('Artillery ',VERSION);
	DrawTextCentre(menuColour,menuStr,middleOfScreen,50);
	DrawTextCentre(menuColour,MOTD,middleOfScreen,60);
	DrawTextCentre(menuColour,'New Game',middleOfScreen,100);
	DrawTextCentre(menuColour,'Exit',middleOfScreen,130);
	
	DrawMap(ColorGreen,IAData.GroundMap);
	RefreshScreen();
	
	if IAData.NewGameRequested then
	begin
		for i:=TEXT_FADE_OUT_TIME downto 0 do
		begin
			ClearScreen(ColorWhite);
			alpha:=i / TEXT_FADE_OUT_TIME;
			menuColour:=RGBAFloatColor(0,0,0,alpha);
			mapColour:=RGBAFloatColor(BlueOf(colorGreen) / 8,GreenOf(colorGreen) / 8,RedOf(colorGreen) / 8,alpha);
			DrawTextCentre(menuColour,menuStr,middleOfScreen,50);
			DrawTextCentre(menuColour,MOTD,middleOfScreen,60);
			DrawTextCentre(menuColour,'New Game',middleOfScreen,100);
			DrawTextCentre(menuColour,'Exit',middleOfScreen,130);
			DrawMap(mapColour,IAData.GroundMap);
			RefreshScreen();
		end;
	end;

end;

procedure HandleMenuInput(var IAData:MainGameData);
var
	buttonNewGame,buttonExit:Boolean;
begin
	buttonNewGame:=buttonClicked(411,96,75,18);
	
	buttonExit:=buttonClicked(423,126,50,18);
	if buttonNewGame then begin IAData.NewGameRequested:=true; IAData.MenuExitRequested:=true; end;
	if buttonExit then begin IAData.ExitRequested:=true; IAData.MenuExitRequested:=true; end;
end;

procedure UpdateMenu(var IAData:MainGameData;var MenuAlpha:Integer);
//Updates the fade effect for the menu.
begin
	if (MenuAlpha < TEXT_FADE_IN_TIME) then
		MenuAlpha+=1;
end;

procedure Menu(var IAData:MainGameData);
var
	MenuAlpha:Integer;
begin
	IAData.ExitRequested:=false;
	IAData.MenuExitRequested:=false;
	IAData.NewGameRequested:=false;
	GenerateTerrain(IAData.GroundMap,round(SCREEN_HEIGHT / 5),fRandBtw(0.7, 1.6), TerrainReduction(Rnd(2)));
	MenuAlpha:=0;
	repeat
		HandleMenuInput(IAData);
		UpdateMenu(IAData,MenuAlpha);
		DrawMenu(IAData,MenuAlpha);
		ProcessEvents();
		if IAData.NewGameRequested then NewGame(IAData);
		if WindowCloseRequested() then IAData.MenuExitRequested:=True;
	until IAData.MenuExitRequested;
end;

procedure DrawMarker(Colour:Color;x:Integer;y:Single;radian:Single;startPoint,Length:Integer;facing:Direction);
//Used to draw the angle markers for the current player;
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
	y,x,textPos:Single;
	name:String;
begin
	for i:=Low(IAData.Player)  to High(IAData.Player) do
	begin
		if (IAData.Player[i].Exists) then
		begin
			x:=IAData.Player[i].pos;
			y:=GroundPosition(Round(x),IAData);
			
			FillCircle(ColorBlack,x,y,3);
			name:=IAData.Player[i].Name;
			textPos:=x-Length(name)*3.5; //Length one one SwinGame character is 7 pixels
			DrawText(Name,ColorBlack,textPos,y-50);
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

procedure DrawPowerBar(IAData:MainGameData;x,y:Integer);
var
	i,Width,previousLocation:Integer;
	maxPower:Single;
	partRect : Rectangle;
begin
	i:=IAData.CurrentPlayer;
	maxPower:=IAData.SPG[Ord(IAData.Player[i].SPG)].MaxGunVelocity;
	Width:=Round(POWER_BAR_WIDTH * IAData.Player[i].ProjectilePower / maxPower);
	previousLocation:=Round(POWER_BAR_WIDTH * IAData.Player[i].PreviousProjectilePower / maxPower);
	partRect := RectangleFrom(0 ,0 ,Width ,POWER_BAR_HEIGHT );
	
	FillRectangleOnScreen(RGBAColor(247,202,47,255),x,y + 5,Width,20);
	FillRectangleOnScreen(ColorBlack,x - 2,y,3,30);
	FillRectangleOnScreen(ColorBlack,x + POWER_BAR_WIDTH - 3,y,3,30);
	DrawVerticalLineOnScreen(ColorBlack,x+previousLocation,y,y+POWER_BAR_HEIGHT);
end;

procedure DrawTraverseBar(IAData:MainGameData;x,y:Integer);
var
	i,Width,previousLocation:Integer;
	Traverse:Single;
	partRect : Rectangle;
begin
	i:=IAData.CurrentPlayer;
	Traverse:=IAData.SPG[Ord(IAData.Player[i].SPG)].TraverseLimit;;
	Width:=Round(TRAVERSE_BAR_WIDTH * IAData.Player[i].TraverseLeft / Traverse);
	partRect := RectangleFrom(0 ,0 ,Width ,TRAVERSE_BAR_HEIGHT );
	
	FillRectangleOnScreen(RGBAColor(0,64,128,255),x,y + 4,Width,4);
	FillRectangleOnScreen(ColorBlack,x - 2,y,3,12);
	FillRectangleOnScreen(ColorBlack,x + POWER_BAR_WIDTH - 3,y,3,12);
end;

procedure DrawShellDesc(ShowingShellDesc:Boolean);
begin
	if ShowingShellDesc then DrawBitmap('shellsdesc.PNG',50,95);
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
	DrawShellDesc(IAData.ShowingShellDesc);
	DrawPowerBar(IAData,445,438);
	DrawTraverseBar(IAData,445,472);
	
	DrawTextOnScreen(nameString,ColorBlack,50,35);
	DrawTextOnScreen(hudString,ColorBlack,50,50);
	DrawTextOnScreen(gunString,ColorBlack,50,60);
	DrawTextOnScreen(speedString,ColorBlack,50,70);
	DrawTextOnScreen(powerString,ColorBlack,50,80);
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
			FillCircle(IAData.Projectile[j].Colour,IAData.Projectile[j].x,SCREEN_HEIGHT-IAData.Projectile[j].y,Round(PROJECTILE_RADIUS * Sqrt(IAData.Projectile[j].PowerUp)));//increases to the Sqrt as it's proportional to the projectile's circular area.
			
			//draw tracers and simulate their fade
		end;
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			//WriteLn('Tracer ',k,' current state: ',IAData.Projectile[j].Tracer[k].Exists, ', Fade timer:',IAData.Projectile[j].Tracer[k].FadeTime);
			if (IAData.Projectile[j].Tracer[k].Exists) then
			begin
				FillCircle(IAData.Projectile[j].Tracer[k].Colour,IAData.Projectile[j].Tracer[k].x,SCREEN_HEIGHT-IAData.Projectile[j].Tracer[k].y,Round(PROJECTILE_RADIUS * Sqrt(IAData.Projectile[j].PowerUp)));
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
			FillRectangle(ColorRed,x+1,y+1,w-2,Round(h/2));
			FillRectangle(ColorGreen,x+1,y+1,fill,Round(h/2));
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

procedure DrawExplosion(IAData:MainGameData);
var
	i:Integer;
	x,y:Single;
	Alpha:Byte;
begin
	for i:=0 to High(IAData.Explosion) do
	begin
		if (IAData.Explosion[i].Exists) then
		begin
		Alpha:=Round(255 * IAData.Explosion[i].FadeTime / EXPLOSION_FADE_TIME);
		x:=IAData.Explosion[i].x;
		y:=IAData.Explosion[i].y;
		FillCircle(RGBAColor(255,127,39,Alpha),x,y,IAData.Explosion[i].Radius);
		end;
	end;
end;

procedure DrawGirls(IAData:MainGameData);
var
	i:Integer;
	x,y:Integer;
begin
	for i:=0 to High(IAData.Girl) do
	begin
		if (IAData.Girl[i].Exists) then
		begin
		//DrawBitmap(IAData.Girl[i].Looks,IAData.Girl[i].x,IAData.Girl[i].y);
		end;
	end;
end;

procedure DrawWindMarker(IAData:MainGameData);
var
	x1,y1,x2,y2,x3,y3,x4,y4,x,y,a,b,z,r:Single;
	windStr,windMagStr,counterStr:String;
begin
	if IAData.Wind.Exists then
		begin
		a:=WIND_MARKER_SIZE;			//Size
		b:=a / 2;						//Sub-size
		z:=Rad(WIND_MARKER_RAD_SIZE);	//Amount of rad to move the points by
		
		r:=Rad(IAData.Wind.Direction);
		x:=SCREEN_WIDTH / 2;
		y:=WIND_MARKER_Y;
		
		x1:=x + a * cos(r);
		y1:=y - a * sin(r);
		x4:=x + b * cos(r + pi);
		y4:=y - b * sin(r + pi);
		x2:=x + b * cos(r + z);
		y2:=y - b * sin(r + z);
		x3:=x + b * cos(r - z);
		y3:=y - b * sin(r - z);
		
		FillTriangleOnScreen(ColorBlack,x1,y1,x2,y2,x3,y3);
		FillTriangleOnScreen(ColorBlack,x4,y4,x2,y2,x3,y3);
		FillCircleOnScreen(IAData.BGColour,x,y,3);
		Str(IAData.Wind.Magnitude / WIND_MAX_MAGNITUDE * 100:0:0,windMagStr);
		windStr:=Concat('Wind: ',windMagStr);
		windStr:=Concat(windMagStr,'%');
		DrawTextCentreOnScreen(ColorBlack,windStr,x,y+30);
		Str(IAData.WindCounter,counterStr);
		DrawTextCentreOnScreen(ColorBlack,counterStr,x,y+20);
	end;
end;

procedure DrawEffects(IAData:MainGameData);
var
	i:Integer;
	x1,y1,w,h:Integer;
	counterStr:String;
begin
	y1:=-EFFECT_HEIGHT;
	h:=SCREEN_HEIGHT+EFFECT_HEIGHT;
	for i:=0 to High(IAData.Effect) do
	begin
		if IAData.Effect[i].Exists then
		begin
			x1:=Round(IAData.Effect[i].x - IAData.Effect[i].Radius);
			w:=Round(IAData.Effect[i].Radius * 2);
			FillRectangle(IAData.Effect[i].Colour,x1,y1,w,h);
			counterStr:=IntToStr(IAData.Effect[i].FadeCounter);
			DrawTextCentre(ColorBlack,counterStr,IAData.Effect[i].x,EFFECT_TEXT_Y);
		end;
	end;
end;

procedure DrawMiniMap(IAData:MainGameData);
var
	i:Integer;
	BGColor,SelectedColor,Clr:Color;
	x:Single;
begin
	FillRectangleOnScreen(ColorBlack,SCREEN_WIDTH - MINIMAP_X,MINIMAP_Y,MINIMAP_WIDTH,1);
	FillRectangleOnScreen(ColorBlack,SCREEN_WIDTH - MINIMAP_X,MINIMAP_Y-Round(MINIMAP_HEIGHT/2),2,MINIMAP_HEIGHT);
	FillRectangleOnScreen(ColorBlack,SCREEN_WIDTH - MINIMAP_X+MINIMAP_WIDTH,MINIMAP_Y-Round(MINIMAP_HEIGHT/2),2,MINIMAP_HEIGHT);
	
	//BGColor:=RGBAColor(40,40,75,255);
	BGColor:=ColorBlack;
	//SelectedColor:=RGBAColor(60,50,240,255);
	SelectedColor:=ColorRed;
	
	for i:=0 to High(IAData.Player) do
	begin
		Clr:=BGColor;
		if (IAData.Player[i].Exists) then 
		begin
			x:= Round(MINIMAP_WIDTH * (IAData.Player[i].pos / Length(IAData.GroundMap)));
			x+= (SCREEN_WIDTH - MINIMAP_X);
			if (i = IAData.CurrentPlayer) then Clr:=SelectedColor;
			
			FillCircleOnScreen(Clr,x,MINIMAP_Y,3);
		end;
	end;
	
	for i:=0 to High(IAData.Effect) do
	begin
		if IAData.Effect[i].Exists then
		begin
			x:= Round(MINIMAP_WIDTH * (IAData.Effect[i].x / Length(IAData.GroundMap)));
			x+= (SCREEN_WIDTH - MINIMAP_X);
			FillRectangleOnScreen(IAData.Effect[i].Colour,Round(x),MINIMAP_Y-Round(MINIMAP_HEIGHT/2),2,MINIMAP_HEIGHT);
		end;
	end;
	
end;

procedure DrawGame(IAData:MainGameData);
begin
	ClearScreen(IAData.BGColour);
	DrawEffects(IAData);
	DrawMap(ColorGreen,IAData.GroundMap);
	DrawWindMarker(IAData);
	DrawPlayers(IAData);
	DrawHealthBars(IAData);
	DrawProjectiles(IAData);
	DrawExplosion(IAData);
	DrawAngleMarkers(IAData);
	DrawDamageText(IAData);
	DrawHUD(IAData);
	DrawMiniMap(IAData);
	//DrawGirls(IAData);
end;

function RandomDirection():Direction;
var
	i:Single;
begin
	i:=Rnd();
	if (i < 0.5) then
	begin
		result:=Direction(0);
	end else begin
		result:=Direction(1);
	end;
end;

procedure InitialiseCombat(var IAData:MainGameData);
var
	i,j,k:Integer;
begin
	IAData.Camera.x:=0;
	IAData.Camera.y:=0;
	IAData.Camera.Tracking:=Player;
	
	//MapType will generate terrain, hence, GenerateTerrain and similar functions need reworking
	GenerateTerrain(IAData.GroundMap,round(SCREEN_HEIGHT / 5),fRandBtw(0.7, 1.6), TerrainReduction(Rnd(2)));
	IAData.currentPlayer:=RandomPlayer(IAData);
	ShowPanel(IAData.ShellPanel);
	IAData.WindCounter:=WIND_START_MIN + Rnd(WIND_START_MAX - WIND_START_MIN);
	IAData.Wind.Exists:=false;
	IAData.EffectCounter:=EFFECT_INTERVAL_MIN + Rnd(EFFECT_INTERVAL_MAX - EFFECT_INTERVAL_MIN);
	IAData.ShowingShellDesc:=false;
	for i:=0 to High(IAData.Player) do
	begin
		IAData.Player[i].Pos:=Rnd(High(IAData.GroundMap)+1);
		IAData.Player[i].Exists:=true;
		IAData.Player[i].GunAngle:=0;
		IAData.Player[i].Facing:=RandomDirection();
		IAData.Player[i].HP:=IAData.SPG[Ord(IAData.Player[i].SPG)].MaxHP;
		IAData.Player[i].PreviousProjectilePower:=0;
		IAData.Player[i].ActiveShellIndex:=0;
		IAData.Player[i].TraverseLeft:=IAData.SPG[Ord(IAData.Player[i].SPG)].TraverseLimit;
		
		IAData.Player[i].Sprite.HullBitmap:=IAData.SPG[Ord(IAData.Player[i].SPG)].Sprite.HullBitmap;
		IAData.Player[i].Sprite.BarrelBitmap:=IAData.SPG[Ord(IAData.Player[i].SPG)].Sprite.BarrelBitmap;
		IAData.Player[i].Sprite.GirlBitmap:=IAData.SPG[Ord(IAData.Player[i].SPG)].Sprite.GirlBitmap;
		
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
		IAData.DamageText[j].FadeTime:=0;
	end;
	
	for j:=0 to High(IAData.Girl) do
	begin
		IAData.Girl[j].Exists:=false;
		IAData.Girl[j].FadeTime:=0;
	end;
	
	for j:=0 to High(IAData.Explosion) do
	begin
		IAData.Explosion[j].Exists:=false;
		IAData.Explosion[j].FadeTime:=0;
	end;
	
	for j:=0 to High(IAData.Effect) do
	begin
		IAData.Effect[j].Exists:=false;
		IAData.Effect[j].FadeCounter:=0;

	end;
	
	IAData.BGColour:=RGBAFloatColor(FRandBtw(0.9,1),FRandBtw(0.9,1),FRandBtw(0.95,1),1);
	SetShellTypes(IAData);
end;

function AngleOfSlope(IAData:MainGameData):Single; //Returns Angle in Rad
//Uses VEHICLE_SLOPE_RADIUS to determine the slope of the current player's position.
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
//Function used to determine how the slope should effect the speed of travel.
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
	
	
	//WriteLn('Current Player: ',IAData.Player[i].Name,', Traverse left: ',IAData.Player[i].TraverseLeft);
	if (IAData.Player[i].TraverseLeft-IAData.Player[i].vel>=0) then
	begin
		IAData.Player[i].pos+=IAData.Player[i].vel;
		IAData.Player[i].TraverseLeft-=abs(IAData.Player[i].vel);
	end;
	
	if (IAData.Player[i].pos > MAP_SIZE) then IAData.Player[i].pos:=SCREEN_WIDTH;
	if (IAData.Player[i].pos < 0) then IAData.Player[i].pos:=0;
end;

procedure UpdatePlayerFacing(var IAData:MainGameData);
//uses acceleration to determine the direction the player is facing.
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
//updates the angle at which the vehicle is tilted due to the slope.
var
	i:Integer;
	slopeAngle:Single;
begin
	i:=IAData.CurrentPlayer;
	//WriteLn('[UpdateVehicleAngle]: Current Player is: ',i);
	slopeAngle:=AngleOfSlope(IAData);
	
	IAData.Player[i].VehicleAngle:=slopeAngle;
end;

function RandomEffect():EffectType;
var
	i:Integer;
begin
	i:=Rnd(Integer(High(EffectType))+1);
	Result:=EffectType(i);
end;

procedure CreateEffect(var IAData:MainGameData);
//Creates an effect at a random location on the map
var
	i,pos:Integer;
	red,green,rb,gb:Single;
	objectCreated:Boolean;
begin
	
	i:=-1;
	objectCreated:=false;
	repeat
		i+=1;
		//WriteLn('[CreateEffect] Effect ',i,' current state: ',IAData.Effect[i].Exists);
		if not (IAData.Effect[i].Exists) then
		begin
			//WriteLn('Creating an Effect!');
			IAData.Effect[i].Exists:=true;
			IAData.Effect[i].EffectOf:=RandomEffect();
			IAData.Effect[i].x:=Rnd(High(IAData.GroundMap)+1);
			IAData.Effect[i].FadeCounter:=EFFECT_DURATION_MIN + Rnd(EFFECT_DURATION_MAX - EFFECT_DURATION_MIN);
			
			case IAData.Effect[i].EffectOf Of
				PowerUp: 
					begin
						IAData.Effect[i].PowerUp:=FRandBtw(EFFECT_POWER_MIN,EFFECT_POWER_MAX);
						IAData.Effect[i].Radius:=Round(EFFECT_BASE_RADIUS * IAData.Effect[i].PowerUp);
						IAData.Effect[i].Multiply:=0;
						IAData.Effect[i].Alpha:=1.0;
						IAData.Effect[i].AlphaAngle:=0;
						red:=EFFECT_POWER_MIN / EFFECT_POWER_MAX;
						gb:=EFFECT_BASE_POWER_GB;
						IAData.Effect[i].Colour:=RGBAFloatColor(red,gb,gb,IAData.Effect[i].Alpha);
					end;
				Multiply: 
					begin
						IAData.Effect[i].PowerUp:=1;
						IAData.Effect[i].Multiply:=EFFECT_MULTIPLY_MIN + Rnd(EFFECT_MULTIPLY_MAX);
						IAData.Effect[i].Radius:=Round(EFFECT_BASE_RADIUS / 2 * IAData.Effect[i].Multiply);
						IAData.Effect[i].Alpha:=1.0;
						IAData.Effect[i].AlphaAngle:=0;
						green:=EFFECT_MULTIPLY_MIN / EFFECT_MULTIPLY_MAX;
						rb:=EFFECT_BASE_MULTIPLY_RB;
						IAData.Effect[i].Colour:=RGBAFloatColor(rb,green,rb,IAData.Effect[i].Alpha);
					end;
				Mirror: 
					begin
						IAData.Effect[i].PowerUp:=1;
						IAData.Effect[i].Radius:=EFFECT_MIRROR_RADIUS;
						IAData.Effect[i].Multiply:=0;
						IAData.Effect[i].Alpha:=1.0;
						IAData.Effect[i].AlphaAngle:=0;
						IAData.Effect[i].Colour:=RGBAColor(230,190,40,255);
					end;
			end;
			objectCreated:=true;
		end;
		if (i=High(IAData.Effect)) then objectCreated:=true;
	until objectCreated;
end;

procedure UpdateEffectCounter(var IAData:MainGameData);
var
	i:Integer;
begin
	for i:=Low(IAData.Effect) to High(IAData.Effect) do
	begin
		if IAData.Effect[i].Exists then
			begin
			IAData.Effect[i].FadeCounter-=1;
			if IAData.Effect[i].FadeCounter<=0 then
			begin
				IAData.Effect[i].Exists:=false;
			end;
		end;
	end;
end;

procedure CreateMultipleEffects(var IAData:MainGameData);
var
	numberEffects,i:Integer;
begin
	numberEffects:= 1 + Rnd(MAX_EFFECTS_CREATED - 1);
	for i:=1 to numberEffects do
	begin
		CreateEffect(IAData);
	end;
end;

procedure UpdateEffects(var IAData:MainGameData);
var
	i:Integer;
begin
	UpdateEffectCounter(IAData);
	IAData.EffectCounter -=1;
	i:=-1;
	
	if (IAData.EffectCounter <= 0) then
	begin
		i:=Rnd(3); //(0..1)
		case i of
			0: CreateEffect(IAData);
			1: CreateEffect(IAData);
			2: CreateMultipleEffects(IAData);
		end;
		IAData.EffectCounter:=EFFECT_INTERVAL_MIN + Rnd(EFFECT_INTERVAL_MAX - EFFECT_INTERVAL_MIN);
	end;
	//WriteLn('[EffectCounter]: Counter is ',IAData.EffectCounter,', i is: ',i);
end;

procedure ChangeWind(var IAData:MainGameData);
begin
	//IAData.Wind.Magnitude:=FRandBtw(WIND_MIN_MAGNITUDE,WIND_MAX_MAGNITUDE);
	IAData.Wind.Magnitude:=Sqr(Rnd()) * (WIND_MAX_MAGNITUDE - WIND_MIN_MAGNITUDE) + WIND_MIN_MAGNITUDE;
	IAData.Wind.TargetDirection:=Rnd(WIND_ANGLE_LIMIT*2) - WIND_ANGLE_LIMIT;
	IAData.Wind.DrawDirection:=IAData.Wind.TargetDirection;
	IAData.Wind.Facing:=RandomDirection();
	
	if (IAData.Wind.Facing = Left) then
		IAData.Wind.DrawDirection += 180;
end;

procedure CreateWind(var IAData:MainGameData);
begin
	IAData.Wind.Exists:=true;
	IAData.Wind.Direction:=0;
	ChangeWind(IAData);
end;

procedure UpdateWindCounter(var IAData:MainGameData);
var
	i:Integer;
begin
	i:=High(IAData.Player) + 1;// number of players
	IAData.WindCounter:=WIND_INTERVAL_MIN * i + Rnd(WIND_INTERVAL_MAX * i - WIND_INTERVAL_MIN * i);
end;

procedure UpdateWind(var IAData:MainGameData);
var
	i:Integer;
begin
	IAData.WindCounter-=1;

	if (IAData.WindCounter <= 0) then 
	begin
		if not IAData.Wind.Exists then
		begin
			CreateWind(IAData);
			UpdateWindCounter(IAData);
		end else begin
			ChangeWind(IAData);
			UpdateWindCounter(IAData);
			//WriteLn('Wind Exists, Direction: ',Ord(IAData.Wind.Facing),', Angle: ',IAData.Wind.Direction:0:2,', Magnitude: ',IAData.Wind.Magnitude:0:2);
		end;
	end;
end;

procedure CyclePlayers(var IAData:MainGameData);
var
	i:Integer;
begin	
	i:=IAData.CurrentPlayer;	
	repeat
	i+=1;
	if (i>High(IAData.Player)) then i:=Low(IAData.Player);
	until IAData.Player[i].Exists;
	if (i>High(IAData.Player)) then i:=Low(IAData.Player);
	IAData.CurrentPlayer:=i;
	SetShellTypes(IAData);
	IAData.Player[i].TraverseLeft:=IAData.SPG[Ord(IAData.Player[i].SPG)].TraverseLimit;
end;

procedure CreateGirl(var IAData:MainGameData);
var
	i,j,pos,middleOfMap,moveDirection,girlPosition:Integer;
	objectCreated:Boolean;
	placement:Direction;
begin
	i:=IAData.CurrentPlayer;
	pos:=Round(IAData.Player[i].pos);
	middleOfMap:=Round(SCREEN_WIDTH/2);
	if (pos > middleOfMap) then
	begin
		placement:=Right;
		moveDirection:=-1;
		girlPosition:=Round(SCREEN_WIDTH / 2 - GIRL_POSITION_OFFSET);
	end else begin
		placement:=Left;
		moveDirection:=1;
		girlPosition:=GIRL_POSITION_OFFSET;
	end;
	
	j:=-1;
	repeat
		j+=1;
		if not (IAData.Girl[j].Exists) then
		begin
			//WriteLn('Drawing a girl!');
			IAData.Girl[j].Exists:=true;
			IAData.Girl[j].Side:=placement;
			IAData.Girl[j].Looks:=IAData.SPG[Ord(IAData.Player[i].SPG)].Sprite.GirlBitmap;
			IAData.Girl[j].FadeTime:=GIRL_FADE_TIME;
			IAData.Girl[i].dx:=GIRL_MOVE_SPEED * moveDirection;
			IAData.Girl[i].x:=girlPosition;
			IAData.Girl[i].y:=GIRL_HEIGHT_OFFSET;
			objectCreated:=true;
		end;
		if (j=High(IAData.Girl)) then objectCreated:=true;
	until objectCreated;
end;

procedure MultiplyProjectile(var IAData:MainGameData; Shot:ProjectileData; number:Integer);
var
	i,j,k:Integer;
	emptySlotFound:Boolean;
begin
	//WriteLn('Multiplying Projectiles by ',number);
	emptySlotFound:=false;
	for i:=0 to number do
		begin
		//WriteLn('i is: ',i);
		j:=-1;
		repeat
		j+=1;
		//WriteLn('Projectile j=',j,' - current state: ',IAData.Projectile[j].Exists);
		if not (IAData.Projectile[j].Exists) then 
		begin
			emptySlotFound:=true;
			IAData.Projectile[j].Exists:=true;
			IAData.Projectile[j].Owner:=Shot.Owner;
			IAData.Projectile[j].Shell:=Shot.Shell;
			IAData.Projectile[j].Colour:=Shot.Colour;
			IAData.Projectile[j].x:=Shot.x;
			IAData.Projectile[j].y:=Shot.y;
			IAData.Projectile[j].PowerUp:=Shot.PowerUp;
			IAData.Projectile[j].dx:=Shot.dx;
			IAData.Projectile[j].dy:=Shot.dy * FRandBtw(EFFECT_MULTIPLY_MIN_DEVIATION,EFFECT_MULTIPLY_MAX_DEVIATION);
			IAData.Projectile[j].IsInEffect:=true;
			//WriteLn('Found slot, making projectile, original dy:',Shot.dy,' post dy:',IAData.Projectile[j].dy);
			for k:=0 to High(IAData.Projectile[j].Tracer) do
			begin
				IAData.Projectile[j].Tracer[k].Exists:=False;
				IAData.Projectile[j].Tracer[k].FadeTime:=0;
			end;	
		end;
		if (j=High(IAData.Projectile)) then emptySlotFound:=true;
		until emptySlotFound;
	end;
end;

procedure FireProjectile(var IAData:MainGameData);
var
	i,j,k:Integer;
	emptySlotFound:Boolean;
	effectivePower,angle:Single;
begin
	i:=IAData.CurrentPlayer;
	emptySlotFound:=false;
	IAData.Player[i].PreviousProjectilePower:=IAData.Player[i].ProjectilePower;
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
		//IAData.Projectile[j].Shell:=ShellType(IAData.ActiveShellIndex);
		IAData.Projectile[j].Shell:=ShellType(IAData.Player[i].ActiveShellIndex);
		IAData.Projectile[j].Colour:=IAData.ShellColour[Ord(IAData.Projectile[j].Shell)];
		IAData.Projectile[j].x:=IAData.Player[i].pos;
		IAData.Projectile[j].y:=IAData.GroundMap[Round(IAData.Player[i].pos)];
		IAData.Projectile[j].dx:=effectivePower * cos(angle);
		IAData.Projectile[j].dy:=effectivePower * sin(angle);
		IAData.Projectile[j].PowerUp:=1;
		if (IAData.Player[i].Facing=Left) then IAData.Projectile[j].dx*=-1;
		
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			IAData.Projectile[j].Tracer[k].Exists:=False;
			IAData.Projectile[j].Tracer[k].FadeTime:=0;
		end;
		//WriteLn('[FireProjectile]: Created Tracers, At end ');	
	end;
	if (j=High(IAData.Projectile)) then emptySlotFound:=true;
	until emptySlotFound;
	
	//CreateGirl(IAData);
	//if (j=-1) then //WriteLn('Maximum number of projectiles reached!');
	IAData.Player[i].ProjectilePower:=0;
	
	CyclePlayers(IAData);
	UpdateWind(IAData);
	UpdateEffects(IAData);
end;

procedure CreateExplosion(var IAData:MainGameData;x,y:Single);
var
	j:Integer;
	objectCreated:Boolean;
begin
	j:=-1;
	repeat
		j+=1;
		if not (IAData.Explosion[j].Exists) then
		begin
			IAData.Explosion[j].Exists:=true;
			IAData.Explosion[j].x:=x;
			IAData.Explosion[j].y:=y;
			IAData.Explosion[j].Radius:=1;
			IAData.Explosion[j].FadeTime:=EXPLOSION_FADE_TIME;
			objectCreated:=true;
		end;
		if (j=High(IAData.Explosion)) then objectCreated:=true;
	until objectCreated;
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
	CreateExplosion(IAData,pos,GroundPosition(Round(pos),IAData));
end;

procedure CreateDamageText(var IAData:MainGameData; effectiveDamage:String;hitPlayer:Integer);
//Damage text is the numbers that are shown when a player is hit
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
	baseDamage:=Round(IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(projectile.Shell)].Damage * projectile.PowerUp);
	//WriteLn('base damage of projectile: ',baseDamage);
	radius:=Round(IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(projectile.Shell)].BlastRadius * projectile.PowerUp);
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
//Simply checks for whether or not the gun should fire and also adjusts the camera accordingly.
var
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	//WriteLn('[UpdateGun]: Current Player is: ',i);	
	if (IAData.Player[i].Exists) then
	begin
		if (IAData.Player[i].FireProjectile) then 
		begin
			FireProjectile(IAData);
			IAData.Camera.Tracking:=Projectile;
		end;
	end;
	//IAData.Camera.Tracking:=Projectile;
end;

procedure PowerProjectile(var Shot:ProjectileData; mult:Single);
//power up the projectile in the "powerup" effect
var
	a:Single;
begin
	a:=FRandBtw(0.5,0.85);
	Shot.PowerUp*=mult;
	if (Shot.PowerUp > EFFECT_POWER_MAX) then Shot.PowerUp:=EFFECT_POWER_MAX;
	//WriteLn('[PowerProjectile], multiplier is: ',mult:0:4, ', Shot Powered up by: ',Shot.PowerUp:0:4,', projectile isIneffect:',Shot.IsInEffect);
	Shot.Colour:=RGBAFloatColor(a,a,a,1);
end;

procedure SimulateProjectiles(var IAData:MainGameData);
var
	i,j,k,explRadius:Integer;
	velocityLoss,windLoss,x1,x2,prevX:Single;
	Alpha:Byte;
	tracerCreated,effectFound:Boolean;
begin
	i:=IAData.CurrentPlayer;
	
	for j:=0 to High(IAData.Projectile) do
	begin
		if (IAData.Projectile[j].Exists) then
		begin
			
			if IAData.Wind.Exists then
			begin
				windloss:=IAData.Wind.Magnitude * cos(Rad(IAData.Wind.TargetDirection));
				if (IAData.Wind.Facing=Left) then windLoss*=-1;
				IAData.Projectile[j].dx+=windLoss;
				IAData.Projectile[j].dy+=IAData.Wind.Magnitude * sin(Rad(IAData.Wind.TargetDirection));
			end;
			
			prevX:=IAData.Projectile[j].x;
			
			IAData.Projectile[j].x+=IAData.Projectile[j].dx;
			IAData.Projectile[j].y+=IAData.Projectile[j].dy;
			
			velocityLoss:=1-IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(IAData.Projectile[j].Shell)].VelocityBleed;
			velocityLoss:=1-(velocityLoss*VELOCITY_LOSS_MULTIPLIER);
			IAData.Projectile[j].dx*=velocityLoss;
			IAData.Projectile[j].dy-=GRAVITY_ACC;  
			
			effectFound:=false;
			for k:=0 to High(IAData.Effect) do
			begin
				if IAData.Effect[k].Exists then
				begin
					x1:=IAData.Effect[k].x - IAData.Effect[k].Radius;
					x2:=IAData.Effect[k].x + IAData.Effect[k].Radius;
					
					if (IAData.Projectile[j].y < (SCREEN_HEIGHT + EFFECT_HEIGHT)) then
					begin
						if between(IAData.Projectile[j].x,x1,x2) or between(IAData.Effect[k].x,IAData.Projectile[j].x,prevX) then
						begin
							//WriteLn('[Pre-parse]Projectile ',j,' isIneffect:',IAData.Projectile[j].IsInEffect);
							if not (IAData.Projectile[j].IsInEffect) then
								begin
								//WriteLn('>>>[Now applying an effect to Projectile]: ',j);
								//Inside this IF Statement is the condition that the projectile
								//is now in an effect, but the fact has not been reconciled yet.
								IAData.Projectile[j].IsInEffect:=true;
								case IAData.Effect[k].EffectOf of
									Mirror: begin IAData.Projectile[j].dx *= -1; IAData.Projectile[j].x+=IAData.Projectile[j].dx; end;
									Multiply: MultiplyProjectile(IAData,IAData.Projectile[j],IAData.Effect[k].Multiply);
									PowerUp: PowerProjectile(IAData.Projectile[j],IAData.Effect[k].PowerUp);
								end;
							end;
							//IAData.Projectile[j].IsInEffect:=true;
							effectFound:=true; //change to false to simulate the problem bug
							//WriteLn('[Parsed Effect]Projectile ',j,', Is in effect ',k,', Type of effect:',Ord(IAData.Effect[k].EffectOf), 'effect found? :',effectFound);
						end
					end;
				end;
			end;
			if not effectFound then IAData.Projectile[j].IsInEffect:=false;
			//WriteLn('[At end of function]Projectile ',j,' isIneffect:',IAData.Projectile[j].IsInEffect);
			
			{ for k:=0 to High(IAData.Effect) do
			begin
				if IAData.Effect[k].Exists then
				begin
					x1:=IAData.Effect[k].x - IAData.Effect[k].Radius;
					x2:=IAData.Effect[k].x + IAData.Effect[k].Radius;
					
					if (IAData.Projectile[j].y < (SCREEN_HEIGHT + EFFECT_HEIGHT)) then
					begin
						if between(IAData.Projectile[j].x,x1,x2) or between(IAData.Effect[k].x,IAData.Projectile[j].x,prevX) then
						begin
							//WriteLn('Projectile ',j,' isIneffect:',IAData.Projectile[j].IsInEffect);
							if not (IAData.Projectile[j].IsInEffect) then
								begin
								//Inside this IF Statement is the condition that the projectile
								//is now in an effect, but the fact has not been reconciled yet.
								IAData.Projectile[j].IsInEffect:=true;
								case IAData.Effect[k].EffectOf of
									Mirror: begin IAData.Projectile[j].dx *= -1; IAData.Projectile[j].x+=IAData.Projectile[j].dx; end;
									Multiply: MultiplyProjectile(IAData,IAData.Projectile[j],IAData.Effect[k].Multiply);
									PowerUp: PowerProjectile(IAData.Projectile[j],IAData.Effect[k].PowerUp);
								end;
							end;
							IAData.Projectile[j].IsInEffect:=true;
							//WriteLn('Projectile ',j,', Is in effect ',k,', Type of effect:',Ord(IAData.Effect[k].EffectOf));
						end else begin
							IAData.Projectile[j].IsInEffect:=false;
						end;
					end;
				end;
			end; }
			
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
					IAData.Projectile[j].Tracer[k].Colour:=IAData.Projectile[j].Colour;
					IAData.Projectile[j].Tracer[k].x:=IAData.Projectile[j].x;
					IAData.Projectile[j].Tracer[k].y:=IAData.Projectile[j].y;
					IAData.Projectile[j].Tracer[k].dx:=0;
					IAData.Projectile[j].Tracer[k].dy:=0;
					tracerCreated:=true;
				end;
				if (k>=High(IAData.Projectile[j].Tracer)) then tracerCreated:=true;
				k+=1;
			until tracerCreated;
			
			if(IAData.Projectile[j].y<IAData.GroundMap[Round(IAData.Projectile[j].x)]) then
			begin
				explRadius:=IAData.SPG[Ord(IAData.Player[i].SPG)].ShellStat[Ord(IAData.Projectile[j].Shell)].BlastRadius;
				//WriteLn('Explosion Radius: ',explRadius);
				DestroyGround(IAData,IAData.Projectile[j].x,Round(explRadius * IAData.Projectile[j].PowerUp));
				DamagePlayers(IAData,IAData.Projectile[j]);
				IAData.Projectile[j].Exists:=false;
			end;
		end;
		
		for k:=0 to High(IAData.Projectile[j].Tracer) do
		begin
			if (IAData.Projectile[j].Tracer[k].Exists) then
			begin
				IAData.Projectile[j].Tracer[k].FadeTime-=1;
				IAData.Projectile[j].Tracer[k].x+=IAData.Projectile[j].Tracer[k].dx;
				IAData.Projectile[j].Tracer[k].y+=IAData.Projectile[j].Tracer[k].dy;
				if (IAData.Projectile[j].Tracer[k].FadeTime<=0) then
				begin
					IAData.Projectile[j].Tracer[k].FadeTime:=0;
					IAData.Projectile[j].Tracer[k].Exists:=false;
				end else begin //Fade Time >0
					Alpha:= Round(255 * (IAData.Projectile[j].Tracer[k].FadeTime / TRACER_FADE_TIME));
					IAData.Projectile[j].Tracer[k].Colour:=RGBAColor(RedOf(IAData.Projectile[j].Colour),BlueOf(IAData.Projectile[j].Colour),GreenOf(IAData.Projectile[j].Colour),Alpha);
					//WriteLn('Alpha of Tracer ',k,': ',Alpha:0:2);
				end;
			end;
		end;
		
	end;
	if not IAData.Projectile[0].Exists then IAData.Camera.Tracking:=Player;
end;

procedure SimulateGirl(var IAData:MainGameData);
var
	i:Integer;
	alpha:Single;
begin
	for i:=0 to High(IAData.Girl) do
	begin
		if (IAData.Girl[i].Exists) then
		begin
			Alpha:=(IAData.Girl[i].FadeTime / GIRL_FADE_TIME);
			IAData.Girl[i].FadeTime-=1;
			IAData.Girl[i].x+=IAData.Girl[i].dx;
			SetOpacity(IAData.Girl[i].Looks,Alpha);
			//WriteLn('Alpha: ',alpha:0:2);
		end;
		
		if (IAData.Girl[i].FadeTime<=0) then
			IAData.Girl[i].Exists:=false;
	end;
end;

procedure SimulateDamageText(var IAData:MainGameData);
//simulates the movement and fading of damage text
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

procedure SimulateExplosion(var IAData:MainGameData);
var
	i:Integer;
begin
	for i:=0 to High(IAData.Explosion) do
	begin
		if (IAData.Explosion[i].Exists) then
		begin
			IAData.Explosion[i].radius+=1;
			IAData.Explosion[i].FadeTime-=1;
		end;
		
		if (IAData.Explosion[i].FadeTime<=0) then
		begin
			IAData.Explosion[i].Exists:=false;
		end;
		
	end;
end;

procedure UpdateExistences(var IAData:MainGameData);
//checks for number of players remaining to determine victory and cycle players
var
	i:Integer;
begin
	//WriteLn('At Existences');
	i:=IAData.CurrentPlayer;
	if not (IAData.Player[i].Exists) then
		CyclePlayers(IAData);
		
	//WriteLn('Before For');
	for i:=0 to High(IAData.Player) do
	begin
		//WriteLn('Inside For');
		if (IAData.Player[i].Exists) then
		begin
			//WriteLn('Player ',i, 'existence');
			if (IAData.Player[i].HP <= 0) then
			begin
				IAData.Player[i].Exists:=false;
				CreateExplosion(IAData,IAData.Player[i].pos,GroundPosition(Round(IAData.Player[i].pos),IAData));
				CyclePlayers(IAData);
				UpdateWind(IAData);
				UpdateEffects(IAData);
				//drawExplosion
			end;
		end;
	end;
end;

procedure SimulateWindMarker(var IAData:MainGameData);
var
	change:Single;
begin
	//IAData.Wind.Direction+=(IAData.Wind.TargetDirection - IAData.Wind.Direction) / WIND_MARKER_MOVE_SPEED;
	change:=(IAData.Wind.DrawDirection - IAData.Wind.Direction) / WIND_MARKER_MOVE_SPEED;
	IAData.Wind.Direction+=change;
	//WriteLn('Target Direction: ',IAData.Wind.DrawDirection:0:1,' Current Direction:',IAData.Wind.Direction:0:1,' Change: ',change:0:1,'Facing: ',Ord(IAData.Wind.Facing));
end;

procedure SimulateObjects(var IAData:MainGameData);
begin
	SimulateProjectiles(IAData);
	SimulateDamageText(IAData);
	SimulateGirl(IAData);
	SimulateExplosion(IAData);
	SimulateWindMarker(IAData);
end;

procedure CheckForVictory(var IAData:MainGameData; var battleEnded:Boolean);
//checks for number of players remaining.
var
	i,players:Integer;
begin
	players:=0;
	for i:=0 to High(IAData.Player) do
	begin
		if IAData.Player[i].Exists then players+=1;
	end;
	if (players<=1) then battleEnded:=true;
end;

procedure UpdateEffectVisuals(var IAData:MainGameData);
var
	i:Integer;
	radIncrement,alpha,effectAlpha,r,g,b:Single;
begin
	radIncrement:= Rad(EFFECT_DEGREE_INCREMENT);
	for i:=0 to High(IAData.Effect) do
	begin
		if IAData.Effect[i].Exists then
		begin
			IAData.Effect[i].AlphaAngle += radIncrement;
			if (IAData.Effect[i].AlphaAngle > 2 * pi) then
				IAData.Effect[i].AlphaAngle:=0;
			alpha:= 0.5 * sin(IAData.Effect[i].AlphaAngle) + 0.5;
			
			case IAData.Effect[i].EffectOf of
				Mirror:  effectAlpha:= (1-EFFECT_MIRROR_ALPHA_MIN) * alpha + EFFECT_MIRROR_ALPHA_MIN;
				else effectAlpha:= (1-EFFECT_ALPHA_MIN) * alpha + EFFECT_ALPHA_MIN;
			end;
			
			r:=RedOf(IAData.Effect[i].Colour) / 255;
			g:=GreenOf(IAData.Effect[i].Colour) / 255;
			b:=BlueOf(IAData.Effect[i].Colour) / 255;
			
			IAData.Effect[i].Colour:=RGBAFloatColor(r,g,b,effectAlpha);
		end;
	end;
end;

procedure UpdateGame(var IAData:MainGameData;var battleEnded:Boolean);
var
	j:Integer;
begin
	UpdateInterface();
	
	//if (RegionClickedID() = 'ShellButton1') then CreateEffect(IAData);//!debug!
	
	UpdatePlayerFacing(IAData);
	UpdateEffectVisuals(IAData);
	//WriteLn('[UpdatedPlayerFacing]');
	SimulateVehicleMovement(IAData);
	//WriteLn('[SimulateVehicleMovement]');
	UpdateVehicleAngle(IAData);
	//WriteLn('[UpdateVehicleAngle]');
	UpdateGun(IAData);
	//WriteLn('[UpdateGame]');
	SimulateObjects(IAData);
	//WriteLn('[SimulateObjects]');
	UpdateExistences(IAData);
	//WriteLn('[UpdateExistences]');
	CheckForVictory(IAData,battleEnded);
end;

procedure ToggleInfoButton(var ShowingShellDesc:Boolean);
begin
	case ShowingShellDesc of
		true: ShowingShellDesc:=false;
		false: ShowingShellDesc:=true;
	end;
end;

procedure UpdateSelectedShell(var IAData:MainGameData);
var
	i:Integer;
begin
	i:=IAData.CurrentPlayer;
	
	case RegionClickedID() of
		'ShellButton1' : IAData.Player[i].ActiveShellIndex:=0;
		'ShellButton2' : IAData.Player[i].ActiveShellIndex:=1;
		'ShellButton3' : IAData.Player[i].ActiveShellIndex:=2;
		'ShellButton4' : IAData.Player[i].ActiveShellIndex:=3;
		'InfoButton' : ToggleInfoButton(IAData.ShowingShellDesc);
		
	end;
	SetShellTypes(IAData);
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
	
	if (IAData.Camera.Tracking = Player) then
	begin
		if (KeyDown(VK_LEFT)) then IAData.Player[i].acc := -VEHICLE_ACC;
		if (KeyDown(VK_RIGHT)) then IAData.Player[i].acc := VEHICLE_ACC;
		if (IAData.Player[i].acc<>0) then IAData.Player[i].BrakesOn := false;
		if (KeyDown(VK_RSHIFT)) then IAData.Player[i].BrakesOn := false;
		if (KeyDown(VK_LSHIFT)) then IAData.Player[i].BrakesOn := false;
		if (KeyDown(VK_SPACE)) then IAData.Player[i].ProjectilePower += POWER_INCREMENT;
		if (KeyReleased(VK_SPACE)) then IAData.Player[i].FireProjectile := true;
		   
		if (KeyDown(VK_UP)) then IAData.Player[i].GunAngle += radIncrement;
		if (KeyDown(VK_DOWN)) then IAData.Player[i].GunAngle -= radIncrement;
		
		if (IAData.Player[i].pos<0) then IAData.Player[i].pos:=0;
		if (IAData.Player[i].pos>High(IAData.GroundMap)) then IAData.Player[i].pos:=High(IAData.GroundMap);
		if (IAData.Player[i].ProjectilePower > maxPower) then IAData.Player[i].ProjectilePower:=maxPower;
		if (IAData.Player[i].GunAngle > radElevation) then IAData.Player[i].GunAngle:=radElevation;
		if (IAData.Player[i].GunAngle < radDepression) then IAData.Player[i].GunAngle:=radDepression;
	end;
end;

procedure MoveCameraToPlayer(var IAData:MainGameData; player:PlayerData);
var
	xOffset,x,y:Single;
begin
	//The camera should not move up and down when tracking players, hence, the y-pos is set.
	xOffset:= -SCREEN_WIDTH / 2;
	x:=player.pos + xOffset;
	y:=0;
	if (x <= 0) then x:=0;
	if (x + SCREEN_WIDTH > High(IAData.GroundMap) + 1) then x:= High(IAData.GroundMap) + 1 - SCREEN_WIDTH;
	if (y > 0) then y:=0;
	IAData.Camera.x += ( x - IAData.Camera.x) / CAMERA_EASE_SPEED;
	IAData.Camera.y += ( y - IAData.Camera.y) / CAMERA_EASE_SPEED;
end;

procedure MoveCameraToProjectile(var IAData:MainGameData);
var
	xOffset,yOffset,x,y:Single;
begin
	//The most important projectile is Index 0. That's all we need right now.
	xOffset:= -SCREEN_WIDTH / 2;
	yOffset:= -SCREEN_HEIGHT / 2;
	x:=IAData.Projectile[0].x + xOffset;
	y:=SCREEN_HEIGHT - IAData.Projectile[0].y + yOffset;
	if (x <= 0) then x:=0;
	if (x + SCREEN_WIDTH > High(IAData.GroundMap) + 1) then x:= High(IAData.GroundMap) + 1 - SCREEN_WIDTH;
	if (y > 0) then y:=0;
	IAData.Camera.x += ( x - IAData.Camera.x) / CAMERA_EASE_SPEED;
	IAData.Camera.y += ( y - IAData.Camera.y) / CAMERA_EASE_SPEED;
	//WriteLn('Projectile y pos: ',IAData.Projectile[0].y:0:2,' target y position: ',y:0:2,' Camera y pos:',IAData.Camera.y:0:2);
end;


procedure UpdateCamera(var IAData:MainGameData);
var
	i:Integer;
begin
	//CameraTracking = (Player, Projectile, Minimap);
	//WriteLn('Camera is tracking to: ',Ord(IAData.Camera.Tracking));
	MoveCameraTo(IAData.Camera.x,IAData.Camera.y);
	i:=IAData.CurrentPlayer;
	case IAData.Camera.Tracking of
		Player: MoveCameraToPlayer(IAData,IAData.Player[i]);
		Projectile: MoveCameraToProjectile(IAData);
	end;
end;

procedure Battle(var IAData:MainGameData);
var
	battleEnded:Boolean;
begin
	battleEnded:=false;
	repeat
		HandleInput(IAData);
		//WriteLn('Handled Input');
		UpdateGame(IAData,battleEnded);
		//WriteLn('Updated Game');
		UpdateCamera(IAData);
		//WriteLn('Updated Camera');
		DrawGame(IAData);
		//WriteLn('Drawn Game');
		RefreshScreen(TARGET_FPS);
		
		if (WindowCloseRequested()) then begin battleEnded:=true; IAData.ExitRequested:=true; end;
	until battleEnded;
end;

procedure SetupPanels(var IAData:MainGameData);
begin
	IAData.ShellPanel:=LoadPanel('shellGroup.txt');
	IAData.ShellDesc:=LoadPanel('shellsdesc.txt');
end;

procedure UpdatePostGame(var IAData:MainGameData);
var
	j:Integer;
begin
	UpdateInterface();
	UpdatePlayerFacing(IAData);
	//WriteLn('[UpdatedPlayerFacing]');
	SimulateVehicleMovement(IAData);
	//WriteLn('[SimulateVehicleMovement]');
	UpdateVehicleAngle(IAData);
	//WriteLn('[UpdateVehicleAngle]');
	UpdateGun(IAData);
	//WriteLn('[UpdateGame]');
	SimulateObjects(IAData);
	//WriteLn('[SimulateObjects]');
end;

procedure ShowVictoryScreen(var IAData:MainGameData);
var
	i,victor,players:Integer;
	victoryStr:String;
begin
	players:=0;
	UpdateExistences(IAData);
	for i:=0 to High(IAData.Player) do
	begin
		if IAData.Player[i].Exists then
		begin
			players:=1;
			victor:=i;
		end;
	end;
	case players of
		1: victoryStr:=Concat(IAData.Player[victor].Name, ' has won!');
		0: victoryStr:='No one has won! It''s a draw!';
	end;
	if not IAData.ExitRequested then begin
		repeat
			ProcessEvents();
			ClearScreen(IAData.BGColour);
			DrawGame(IAData);
			HandleInput(IAData);
			UpdatePostGame(IAData);
			UpdateCamera(IAData);
			DrawTextCentreOnScreen(ColorBlack,victoryStr,SCREEN_WIDTH/2,200);
			DrawTextCentreOnScreen(ColorGrey,'Click to continue',SCREEN_WIDTH/2,220);
			RefreshScreen(TARGET_FPS);
		until MouseClicked(LeftButton);
		MoveCameraTo(0,0);
	end;
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
		if not IAData.ExitRequested then
		begin
			InitialiseCombat(IAData);
			Battle(IAData);
			ShowVictoryScreen(IAData);
			if WindowCloseRequested() then IAData.ExitRequested:=True;
		end;
	until IAData.ExitRequested=True;
	ReleaseAllResources();
end;

begin
	Main();
end.