/*
 * Fish Scala:
 *
 * Quick implementation of Go Fish.
 */
import util.Random

/*
 * Configure game details here.
 */
object Global {
  val maxCard = 13
  val sizeHand = 6
  val suits = List("Spades", "Diamonds", "Clubs", "Hearts")
}

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
class Player(who: String, out: String => Unit, choose: Player => (String, String) => Int) {
  val name: String = who
  val write: String => Unit = out
  val getChoice: (String, String) => Int = choose(this)

  var hand: List[Card] = List[Card]()
  var books: List[Int] = List[Int]()
  def show() = write(name + "\n" + hand.map((c: Card) => "\n" + c.name))
  def hasCards(): Boolean = hand.length > 0

  /*
   * Ask for cards of a particular rank.
   */
  def ask(i: Int, opp: Player): List[Card] = {
    val give = hand.filter((c: Card) => c.rank == i)
    opp.add(give)
    hand = hand diff(give)
    give
  }

  /*
   * See if the player has all the cards of any rank.
   * If so, extract them into a book.
   */
  def extractBooks() = {
    (1 to Global.maxCard).foreach((i: Int) => {
      val possibleBook = hand.filter((c: Card) => c.rank == i)
      if (possibleBook.length == Global.suits.length) {
        books ::= i
        hand = hand diff(possibleBook)
      }
    })
  }

  /*
   * Add cards, both individually and as a list.
   */
  def add(c: Card) = {
    hand ::= c
    extractBooks
  }
  def add(l: List[Card]) = {
    hand = hand ::: l
    extractBooks
  }
}

object GoFish extends App {
  /*
   * join() is just a utility function to print lists cleanly.
   */
  def join[T](list : List[T]) = list match {
    case xs if xs.size < 3 => xs.mkString(" and ")
    case xs => xs.init.mkString(", ") + ", and " + xs.last
  }

  /*
   * Stub function for output in case we re-target later.
   */
  def output(s: String) = println(s)

  /*
   * Stub function to eat output.
   */
  def noOutput(s: String) = 0

  /*
   * getChoice() asks for a card rank until it gets a
   * workable answer.
   */
  def getChoice(p: Player)(q: String, again: String): Int = {
    var repeat = true
    var askFor = 0
    while (repeat) {
      p.write(q + "  ")
      try {
        askFor = readInt
        repeat = p.hand.filter((c: Card) => c.rank == askFor).length == 0
      } catch {
        case _ => repeat = true
      }
      if (repeat) {
        p.write(again)
      }
    }
    askFor
  }

  /*
   * aiChoice() randomly picks a number of the available cards.
   */
  def aiChoice(p: Player)(q: String, again: String): Int = {
    var ranks = p.hand.map((c: Card) => c.rank)
    ranks = ranks.sortWith(_ > _)
    ranks.foldRight(List.empty[Int]) {
      case (a, b) =>
      if (!b.isEmpty && b(0) == a) {
        b
      } else {
        a :: b
      }
    }
    ranks(rand.nextInt(ranks.length))
  }

  /*
   * Initialize variables
   */
  var rand = new Random
  var deck = Global.suits.map((s: String) => (1 to Global.maxCard).map((i: Int) => new Card(i, s))).flatten
  deck = Random.shuffle(deck)

  // Boring players, for now.
  var players = List[Player]()
  players ::= new Player("First Player", output, getChoice)
  players ::= new Player("Second Player", noOutput, aiChoice)
  players = Random.shuffle(players)

  /*
   * Deal the cards.
   */
  (1 to Global.sizeHand).foreach((i: Int) => {
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
    p.show

    // Handle card request.
    val askFor = p.getChoice("Ask for?", "Please try again.")
    val asked = opponent.ask(askFor, p)
    if (asked.length == 0) {
      // Opponent has no cards
      val cname = if (deck.length > 0) {
        // Cards remain.
        val c = deck.head
        p.add(c)
        deck = deck.tail
        c.name
      } else {
        // No cards remain.
        ""
      }
      output("Go Fish!  " + cname)
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
    output(p.name + ":  " + join[Int](p.books))
  })
}

