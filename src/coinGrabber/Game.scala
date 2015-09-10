

package coinGrabber


class Game {
  

  /**
   * Method to find new location of player
   * using original location and direction.
   * @param location
   * @param direction
   * @return new location
   */
  def charToLocation(location: Array[Int],direction: Char) : Array[Int] = {
   
    var locationNew = Array[Int](2)
    if(direction == 'U' || direction == 'u') {
      locationNew = Array(location(0) - 1 , location(1))
    }
    else if(direction == 'D' || direction == 'd') {
      locationNew = Array(location(0) + 1 , location(1) )
    }
    else if(direction == 'L' || direction == 'l') {
      locationNew = Array(location(0)  , location(1) - 1)
    }
    else if(direction == 'R' || direction == 'r') {
      locationNew = Array(location(0) , location(1) + 1)
    }
    return locationNew
  }
  
  /**
  * Checks whether location is within the 5X5 board.
  * @param location
  * @return boolean
  */
  def isLegalLocation(location: Array[Int]) : Boolean = {
    var row = location(0)
    var column = location(1)
    if (row >= 0 && row < 5 && column >= 0 && column < 5)
       return true
    return false
  }

  /**
  * Checks whether move is legal.
  * @param location
  * @return boolean
  */
  def isLegalMove(location:Array[Int]) : Boolean = {
    if(!isLegalLocation(location.clone())){
     return false
    }     
   return true
   }
  
  /**
   * Returns a list of all legal moves.
   * @param player
   * @param state
   * @return
   */
  def possibleMoves(player: Char, state: State): List[Char] = {
    
    var moves  = List[Char]()      
    var newLocation = Array[Int](2)
    var newState = new State(state.boardState.map(_.clone()),state.humanLocation.clone(),state.compLocation.clone(), state.humanMoney, state.compMoney)
    if(player == 'h'){
      newLocation = charToLocation(newState.humanLocation.clone(), 'U').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'c'){
        moves ++= List('U')
      }
      newLocation = charToLocation(newState.humanLocation.clone(), 'D').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'c'){
        moves ++= List('D')
      }
      newLocation = charToLocation(newState.humanLocation.clone(), 'L').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'c'){
        moves ++= List('L')
      }
      newLocation = charToLocation(newState.humanLocation.clone(), 'R').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'c'){
        moves ++= List('R')
      }
    }
    else if(player == 'c'){
      newLocation = charToLocation(newState.compLocation.clone(), 'U').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'h'){
        moves ++= List('U')
      }
      newLocation = charToLocation(newState.compLocation.clone(), 'D').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'h'){
        moves ++= List('D')
      }
      newLocation = charToLocation(newState.compLocation.clone(), 'L').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'h'){
        moves ++= List('L')
      }
      newLocation = charToLocation(newState.compLocation.clone(), 'R').clone()
      if(isLegalMove(newLocation) && newState.boardState(newLocation(0))(newLocation(1)) != 'h'){
        moves ++= List('R')
      }
    }
   return moves
  }
  
  
  /**
  * Returns next state of the game based on current state, 
  * player which is going to move and direction of player.
  * @param player
  * @param direction
  * @param state
  * @return next state of game
  */
  def nextState(player: Char, direction: Char, state: State): State = {
         
    var newState = new State(state.boardState.map(_.clone()),state.humanLocation.clone(),state.compLocation.clone(), state.humanMoney, state.compMoney)
    var moves  = List[Char]()
    var newLocation = Array[Int](2)
    var money  = 0
    var newBoardState = newState.boardState.map(_.clone)
    var newHumanLocation = newState.humanLocation.clone
    var newCompLocation = newState.compLocation.clone
    var newHumanMoney = newState.humanMoney
    var newCompMoney = newState.compMoney
    
    if(player == 'h'){
       newHumanLocation = charToLocation(state.humanLocation.clone(), direction)
       money = state.boardState(newHumanLocation(0))(newHumanLocation(1))
       newBoardState(newState.humanLocation(0))(newState.humanLocation(1)) = 0
       newBoardState(newHumanLocation(0))(newHumanLocation(1)) = 'h'
       newHumanMoney = newState.humanMoney + money
      
    }
    else if(player == 'c'){
      newCompLocation = charToLocation(state.compLocation.clone(), direction)
      money = newState.boardState(newCompLocation(0))(newCompLocation(1))
      newBoardState(newState.compLocation(0))(newState.compLocation(1)) = 0
      newBoardState(newCompLocation(0))(newCompLocation(1)) = 'c'
      newCompMoney = newState.compMoney + money     
    }
    
   var finalState = new State(newBoardState,newHumanLocation, newCompLocation,newHumanMoney,newCompMoney)
   return finalState
  }
  
  /**
   * Chooses the best move for computer using evaluate method and returns it.
  * @param depth
  * @param state
  * @return best move for computer
  */
  def chooseMove(depth: Int, state: State): Char = {
     var allMoves  = List[Char]()
     var newState = new State(state.boardState.map(_.clone()),state.humanLocation.clone(),state.compLocation.clone(), state.humanMoney, state.compMoney)
     allMoves = possibleMoves('c', newState)
     var bestMove : Char = 'u'
     var max: Int = -1000000
     for(move <- allMoves){
       var moveValue = evaluateMove('c', move, depth, newState)
       if(moveValue > max){
         max = moveValue
         bestMove = move
       }
     }
     return bestMove
  }
  
  /**
  * Returns the value of the move (larger values are better) for the given player.
  * @param player
  * @param direction
  * @param depth
  * @param state
  * @return value
  */
  def evaluateMove(player: Char, direction: Char, depth: Int, state: State): Int = {

    var moves  = List[Char]()
    var max = -100000    
    if(depth == 0){
      return heuristic(player, state)
    }
    var newState = nextState(player, direction, state)
    var boardcenter = newState.boardState(2)(2)
    if(boardcenter == 'c' || boardcenter == 'h'){
      return heuristic(player, newState)
    }
    if(player == 'c'){
      moves = possibleMoves('h', newState)
      for(move <- moves){
        var value = evaluateMove('h', move, depth - 1, newState)
        if(max < value){
          max = value
        } 
      }
    }
    
    if(player == 'h'){
      moves = possibleMoves('c', newState)
        for(move <- moves){
        var value = evaluateMove('c', move, depth - 1, newState)
          if(max < value){
          max = value
        } 
      }
    }
   return -1 * max
  }
  
  /**
  * Finds the value of the given state for the given player.
  * @param player
  * @param state
  * @return Heuristic value
  */
  def heuristic(player: Char, state: State): Int = {
    var newState = new State(state.boardState.map(_.clone()),state.humanLocation.clone(),state.compLocation.clone(), state.humanMoney, state.compMoney)
    if(player == 'c'){
      if(newState.compLocation == Array(2,2)){
        if(newState.compMoney > 100){
          if(newState.compMoney > newState.humanMoney){
            return 100000
          }
          else{
           return -100000
          }
        }
        else{
          return -100000
        }
      }
      else{
        return newState.compMoney - newState.humanMoney
      }
    }
    else{
      if(newState.humanLocation == Array(2,2)){
        if(newState.humanMoney > 100){
          if(newState.humanMoney > newState.compMoney){
            return 100000
          }
          else{
            return -100000
          }
        }
        else{
          return -100000
        }
      }
      else{
        return newState.humanMoney - newState.compMoney
      }
    }
  }

  /**
  * Prints the board in its current state.
  * @param state
  */
  def printBoard(state: State){
    for( i <- 0 until 5){
       println()
       println("------------------------")
        for ( j <- 0 until 5){

          if(state.boardState(i)(j) == 'c'){
            print(" c")
          }
          else if(state.boardState(i)(j) == 'h'){
            print(" h")
          }
          else if(state.boardState(i)(j) == 5){
            print(" 5")
          }
          else if(state.boardState(i)(j) == 0){
            print(" 0")
          }
          else if(state.boardState(i)(j) == 1){
            print(" 1")
          }
          else{
            print(state.boardState(i)(j))
          }
          print(" | ")
        }
    }
    println()
    println("------------------------")
  }
}