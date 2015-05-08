<Query Kind="Statements" />

// max number of horizontal/vertical walkable tiles
var maze_width = 30;
var maze_height = 30;
const char wall='#';
const char start='s';
const char open=' ';
const char end='e';

var rnd = new Random(2);
var width = 2*maze_width+1;
var height = 2*maze_height+1;
var maze = new char[width,height];
//var moves =new List<int>();
for(var x=0;x<height;x++){
	 for(var y=0;y<width;y++){
		  maze[x,y]=wall;
	 }
}
maze.Dump();
var x_pos = 1;
var y_pos = 1;
maze[x_pos,y_pos]=start;
var moves=new Stack<int>();

moves.Push(y_pos+(x_pos*width));
//maze.Dump();
var firstPop=true;
while(moves.Count>0){
	
	//(new{x_pos,y_pos}).Dump("location");
	 var possible_directions = string.Empty;
	 if( x_pos+2<height-1 && maze[x_pos+2,y_pos]==wall ){
		   possible_directions += "S";
	 }
	 //"south".Dump("checked");
	 if( x_pos-2>=0 && maze[x_pos-2,y_pos]==wall &&    x_pos-2!= height-1){
		   possible_directions += "N";
	 }
	// "north".Dump("checked");
	 if(y_pos-2>=0 && maze[ x_pos, y_pos-2]==wall &&    y_pos-2!= width-1){
		   possible_directions += "W";
	 }
	 //"west".Dump("checked");
	 if( y_pos+2<= width-1 && maze[ x_pos, y_pos+2]==wall ){
		   possible_directions += "E";
	 }
	 //"east".Dump("checked");
	 if( possible_directions.Length>0){
	 		possible_directions.Dump("possibles");
		   var move = rnd.Next(0,possible_directions.Length).Dump("going");
		   
		  switch ( possible_directions[ move].ToString()){
			   case "N":  maze[ x_pos-2, y_pos]=open;
						  maze[ x_pos-1, y_pos]=open;
						  x_pos -=2;
						 break;
			   case "S":  maze[ x_pos+2, y_pos]=open;
						  maze[ x_pos+1, y_pos]=open;
						  x_pos +=2;
						 break;
			   case "W":  maze[ x_pos, y_pos-2]=open;
						  maze[ x_pos, y_pos-1]=open;
						  y_pos -=2;
						 break;
			   case "E":  maze[ x_pos,y_pos+2]=open;
						  maze[ x_pos, y_pos+1]=open;
						  y_pos +=2;
						 break;        
		  }
		  moves.Push( y_pos+( x_pos* width));
	 }
	 else{
	 		if(firstPop){
			firstPop=false;
			maze[x_pos,y_pos]=end;
			}
		   var back = moves.Pop();
		   decimal stackX=back/width;
		   x_pos =(int) Math.Floor( stackX);
		   y_pos =  back% width;
	 }
}
maze.Dump("final");
