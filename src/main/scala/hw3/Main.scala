package hw3

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

object Main {
  def mean(vector: List[Double]): Double ={
    var res = 0.0
    for(i <- vector) {
      res += i
    }
    res/=vector.size
    res
  }
  def standardDeviation(vector: List[Double]): Double = {
    if(vector.isEmpty)
      throw new Exception("Argument is an empty list")
    if(vector.size==1)
      return 0.0
    var numenator = 0.0
    val _mean = mean(vector)
    for (i <- vector){
      numenator+= scala.math.pow(i - _mean, 2)
    }
    scala.math.sqrt(numenator/(vector.size))
  }

  def letterFrequencyRanking(corpus: String): String ={
    var weights = Map[Char, Int]()
    for(i<-corpus){
      if(i.isLetter) {
        val exists = weights.exists(x => x._1 == i)
        if (exists) {
          val value = weights(i) + 1
          weights += (i.toLower -> value)
        }
        else {
          weights += (i.toLower -> 1);
        }
      }
    }
    if(weights.isEmpty) ""
    weights = ListMap(weights.toSeq.sortBy(_._1):_*)
    var res = new StringBuilder()
    for((k,v)<-weights){
      val elem = weights.maxBy(_._2)._1
      res += elem
      weights-= elem
    }
    res.toString()
  }

  def romanji(katakana: String): String = {
    var res = new StringBuilder()
    var isDoubledSingle = false
    var isDoubledNX = false

    for(i<-katakana){
      //check if the current symbol is in the basic list
      if(Katakana.symbols.exists(x=>x._1==i.toLower)){

        for(j<-Katakana.symbols(i)){

          if(isDoubledSingle){
            res+=j
          }
          if(isDoubledNX){
            res+='n'
          }
          res+=j
          isDoubledSingle = false
        }
      }
      else {
        //checking the pattern of "prolonging"
        i match {
          case 'ャ' => {
            if (res(res.size - 1) == 'i') {
              res(res.size - 1) = 'y'
              res += 'a'
            }
            isDoubledSingle = false
          }
          case 'ュ' => {
            if (res(res.size - 1) == 'i') {
              res(res.size - 1) = 'y'
              res += 'u'
            }
            isDoubledSingle = false
          }
          case 'ョ' => {
            if (res(res.size - 1) == 'i') {
              res(res.size - 1) = 'y'
              res += 'o'
            }
            isDoubledSingle = false
          }
          case 'ッ' => {
            isDoubledSingle = true
          }
          case 'ン' => {
            isDoubledNX = true
          }
          case 'ー' => {
            res(res.size-1) = Katakana.longVowels(res(res.size - 1))
          }
            //error case
          case whoa => throw new Exception("The argument contains symbol(s) not from katakana")
        }
      }
    }

    res.toString()

  }



  //def gray(bits: Int): List[String]
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}