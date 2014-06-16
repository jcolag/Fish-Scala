/*
 * Fish Scala:
 *
 * Quick implementation of Go Fish.
 */
import util.Random

/*
 * Card represents a playing card, of course.
 */
class Card(v: Int, s: String) {
  val rank: Int = v
  val suit: String = s

  // String interpolation doesn't work?'
  def name(): String = "The " + rank + " of " + suit
}

/*
 * Player keeps track of each player's state.
 */
class Player(who: String) {
  val name: String = who
  var hand: List[Card] = List[Card]()
  var books: List[Int] = List[Int]()
  def show(): String = name + "\n" + hand.map((c: Card) => "\n" + c.name)
  def hasCards(): Boolean = hand.length > 0
  def ask(i: Int): List[Card] = hand.filter((c: Card) => c.rank == i)
  def extractBooks() = {
    /*
     * See if the player has all the cards of any rank.
     * If so, extract them into a book.
     */
    (1 until maxCard + 1).foreach((i: Int) => {
      val possibleBook = hand.filter((c: Card) => c.rank == i)
      if (possibleBook.length == suits.length) {
        books ::= i
        hand = hand diff(possibleBook)
      }
    })
  }
  def add(c: Card) = {
    hand ::= c
    extractBooks
  }
  def add(l: List[Card]) = {
    hand = hand ::: l
    extractBooks
  }
}

/*
 * join() is just a utility function to print lists cleanly.
 */
def join[T](list : List[T]) = list match {
  case xs if xs.size < 3 => xs.mkString(" and ")
  case xs => xs.init.mkString(", ") + ", and " + xs.last
}

def getChoice(q: String): Int = {
  var repeat = true
  var askFor = 0
  while (repeat) {
    print(q + "  ")
    try {
      val askFor = readInt
      repeat = false
    } catch {
      case _ => {
        repeat = true
        println("Please try again.")
      }
    }
  }
  askFor
}

/*
 * Initialize variables
 */
val maxCard = 13
val sizeHand = 6
val suits = List("Spades", "Diamonds", "Clubs", "Hearts")
var deck = suits.map((s: String) => (1 until maxCard + 1).map((i: Int) => new Card(i, s))).flatten
deck = Random.shuffle(deck)

// Boring players, for now.
var players = List[Player]()
players ::= new Player("First Player")
players ::= new Player("Second Player")
players = Random.shuffle(players)

/*
 * Deal the cards.
 */
(0 until sizeHand).foreach((i: Int) => {
  players.foreach((p: Player) => {
    p.add(deck.head)
    deck = deck.tail
  })
  players = players.tail ::: List(players.head)
})

/*
 * Play turn by turn.
 */
var continue = true
while (continue) {
  val p = players.head
  val opponent = players.tail.head
  println(p.show)

  // Handle card request.
//  print("Ask for? ")
  val askFor = getChoice("Ask for?") // readInt
  val asked = opponent.ask(askFor)
  if (asked.length == 0) {
    // Opponent has no cards
    print("Go Fish!  ")
    if (deck.length > 0) {
      // Cards remain.
      println(deck.head.name)
      p.add(deck.head)
      deck = deck.tail
    } else {
      // No cards remain.
      println("")
    }
  } else {
    // Transfer the card(s).
    opponent.hand = opponent.hand diff(asked)
    p.add(asked)
  }

  /*
   * Next player, but end if any player is out of cards.
   */
  players = players.tail ::: List(players.head)
  players.foreach((p: Player) => continue &= p.hasCards)
}

/*
 * Print out each player's books.
 */
players.foreach((p: Player) => {
  println(p.name + ":  " + join[Int](p.books))
})

