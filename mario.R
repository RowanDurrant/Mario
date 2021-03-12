###God fucking damn it mario
###This is Ollie's fault

##Game --------------------

mario = function(){
  # set up objects
  mario = setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("status", "x", "y"))
  mario$status = "alive"
  mario$x = 4.5
  mario$y = 2.5
  
  # obstacles
  objects = read.csv("obstacles.csv")
  names(objects) = c("x1", "x2", "y1", "y2")
  
  #ground
  ground = setNames(data.frame(matrix(ncol = 4, nrow = 4)), c("x1", "x2", "y1", "y2"))
  ground$x1[1] = -5
  ground$x2[1] = 64
  ground$y1[1] = -1
  ground$y2[1] = 2
  
  ground$x1[2] = 65
  ground$x2[2] = 80
  ground$y1[2] = -1
  ground$y2[2] = 2
  
  ground$x1[3] = 82
  ground$x2[3] = 145
  ground$y1[3] = -1
  ground$y2[3] = 2
  
  ground$x1[4] = 146
  ground$x2[4] = 196
  ground$y1[4] = -1
  ground$y2[4] = 2
  
  #that flagpole thing at the end
  flag = setNames(data.frame(matrix(ncol = 4, nrow = 1)), c("x1", "x2", "y1", "y2"))
  flag$x1[1] = 191
  flag$x2[1] = 192
  flag$y1[1] = 2
  flag$y2[1] = 6
  
  par(bg = "skyblue")
  
print("d = right")
print("a = left")
print("w = jump up")
print("e = jump right")
print("q = jump left")
print("p to quit")
print("good luck")


  while(mario$status == "alive"){ 
   
  plot(mario$y ~ mario$x, xlim = c(mario$x - 5, mario$x + 15), ylim = c(0,12), pch = 16, cex = 3, col = "blue")
  for(i in 1:nrow(objects)){
    rect(objects$x1[i], objects$y1[i],objects$x2[i], objects$y2[i], col = "brown")
  }  
    for(i in 1:nrow(ground)){
      rect(ground$x1[i], ground$y1[i],ground$x2[i], ground$y2[i], col = "green")
    }
    rect(flag$x1[1], flag$y1[1], flag$x2[1], flag$y2[1], col = "red")

    while(nrow(ground[ground$y2 == mario$y - 0.5 & mario$x > ground$x1 & mario$x < ground$x2,]) == 0 &
          nrow(objects[objects$y2 == mario$y - 0.5 & mario$x > objects$x1 & mario$x < objects$x2,]) == 0 ){
      mario$y = mario$y - 1
      Sys.sleep(0.5)

      plot(mario$y ~ mario$x, xlim = c(mario$x - 5, mario$x + 15), ylim = c(0,12), pch = 16, cex = 3, col = "blue")
      for(i in 1:nrow(objects)){
        rect(objects$x1[i], objects$y1[i],objects$x2[i], objects$y2[i], col = "brown")
      }  
      for(i in 1:nrow(ground)){
        rect(ground$x1[i], ground$y1[i],ground$x2[i], ground$y2[i], col = "green")
      }
      rect(flag$x1[1], flag$y1[1], flag$x2[1], flag$y2[1], col = "red")
      
      if(mario$y < 0){
        print("You died!")
        break()
      }
    }
    
  if(mario$y < 0){
    break()
  }
    
    if(mario$x > 191){
      print("You win!")
      break()
    }
  move = readline(prompt = "select action")
  #right
  if(move == "d"){
    if(nrow(objects[objects$x1 == mario$x + 0.5 & objects$y1 < mario$y & objects$y2 > mario$y,]) == 0){
      mario$x = mario$x + 1
      } else{}
    
  }
  
  #left
  else if(move == "a"){
    if(nrow(objects[objects$x1 == mario$x - 0.5 & objects$y1 < mario$y & objects$y2 > mario$y,]) == 0){
      mario$x = mario$x - 1
    } else{}
  }
  
  #jump up
  else if(move == "w"){
    mario$y = mario$y + 2
    
  }
  
  #jump right
  else if(move == "e"){
    mario$x = mario$x + 2
    mario$y = mario$y + 2
    
  }
  
  #jump left
  else if(move == "q"){
    mario$x = mario$x - 2
    mario$y = mario$y + 2
  }
  
  #quit
  else if(move == "p"){break()}
  

  

}
  
  
}

mario()
