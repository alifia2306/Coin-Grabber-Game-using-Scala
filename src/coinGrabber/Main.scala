package coinGrabber

import scala.io.StdIn.readLine
import coinGrabber.Game
import scala.util.control._
object Main {
  var depth = 10
  var game = new Game()
  
  /**
  * Main Method
  * @param args
  */
  def main(args: Array[String]) {
    var gameEnd = false 
    var boardState = Array(Array('h', 5, 10, 5, 25),
              Array(5, 10, 25, 10, 5),
              Array(10, 25, 1, 25, 10),
             Array(5, 10, 25, 10, 5),
             Array (25, 5, 10, 5, 'c'))
  
    var humanLocation = Array(0,0)
    var compLocation = Array(4,4)
    var humanMoney = 0
    var compMoney = 0
    var state = new State(boardState, humanLocation, compLocation, humanMoney, compMoney)
     
    game.printBoard(state)
    println("Do you want to play first? (y/n)")
    var choice = readLine.charAt(0)
    var player = 'r'
    while(choice != 'y' && choice != 'n'){
      print("Invalid Input , try again!")
      println("Do you want to play first? (y/n)")
      choice = readLine.charAt(0)
     }
    
     if(choice == 'y'){
       player = 'h'
     }
     if(choice == 'n'){
       player = 'c'
     }
      
     while(!gameEnd){
       if(player == 'h'){
         println("Please choose move from 'U','D','R' or 'L'")
         var direction = readLine.charAt(0)
         direction = direction.toUpper
         var possibleMovesList = game.possibleMoves('h', state)
         while(!(possibleMovesList.contains(direction))){
           println("Invalid Move, try again")
           println("Please choose move from 'U','D','R' or 'L'")
           direction = readLine.charAt(0)
           direction = direction.toUpper
         }
         
         state = game.nextState('h', direction, state)
         game.printBoard(state)
         println("Your Money : " + state.humanMoney + "; Computer Money : " + state.compMoney)
         println()
         player = 'c'
       }
       else{
         println("Computer's turn")
         var bestMove = game.chooseMove(depth, state)
         state = game.nextState('c', bestMove, state) 
         println("Computer Played:  " + bestMove)
         game.printBoard(state)
         println("Your Money : " + state.humanMoney + "; Computer Money : " + state.compMoney)
         println()
         player = 'h'
       }
       if(state.boardState(2)(2) == 'c' || state.boardState(2)(2) == 'h') {
         gameEnd = true
       }

    }

    if(state.boardState(2)(2) == 'c'){
      if(state.compMoney > state.humanMoney && state.compMoney > 100){
        print("You Lose!")
      }
    else{
        print("You Win!")
      }
    }
    
    else{
      if(state.humanMoney > state.compMoney && state.humanMoney > 100){
        print("You Win!")
      }
      else{
        print("You Lose!")
      }
    }   
  }
}