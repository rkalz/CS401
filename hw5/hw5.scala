import io.Source.fromFile
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/*
0 - Assignment
1 - Variable ($X)
2 - Operator +
3 - Operator -
4 - Operator *
5 - Operator /
6 - Operator sqrt
7 - Type double
8 - Type int
*/

class Token(var str: String, var typ: Int) {}

class Tokenizer(file: String) {
    var tokens = ArrayBuffer[Token]()

    val lines = fromFile(file).getLines()
    while (lines.hasNext) {
        val words = lines.next().split(Array(' ', '(', ',', ')'))
        words.foreach( word =>
            if (word == "−>" || word == "->") { tokens.append(new Token(word, 0)) }
            else if (word contains "$") { tokens.append(new Token(word, 1)) }
            else if (word == "+") { tokens.append(new Token(word, 2)) }
            else if (word == "−") { tokens.append(new Token(word, 3)) }
            else if (word == "*") { tokens.append(new Token(word, 4)) }
            else if (word == "/") { tokens.append(new Token(word, 5)) }
            else if (word == "sqrt") { tokens.append(new Token(word, 6)) }
            else if (word contains "double") { tokens.append(new Token(word, 7)) }
            else if (word contains "int") { tokens.append(new Token(word, 8)) }        )
    }
}

class Expression(var operator: String, var left: String, var right: String, var result: String) {}

class TypeChecker(file: String) {
    var exps = ArrayBuffer[Expression]()
    var vars: Map[String, String] = Map()

    val tokenizer = new Tokenizer(file)
    val iter = tokenizer.tokens.iterator
    while(iter.hasNext) {
        val operator = iter.next().str
        val inputOne = iter.next().str
        val inputTwo = iter.next().str
        if (inputTwo == "−>" || inputTwo == "->") { exps.append(new Expression(operator, inputOne, "", iter.next().str)) }
        else {
            iter.next() // Skip assignment after second variable
            exps.append(new Expression(operator, inputOne, inputTwo, iter.next().str))
        }
    }

    def value(variable: String) : String = {
        if (variable == "int" || variable == "&int") return "int"
        else if (variable == "double" || variable == "&double") return "double"
        else if (variable == "") return ""
        else if (vars.contains(variable)) return vars(variable)

        val exp = exps.find(_.result == variable)
        exp match {
            case Some(i) => printf("")
            case None => throw new IllegalStateException("Variable not found")
        }

        val left = this.value(exp.get.left)
        val right = this.value(exp.get.right)
        if (left != right && right != "") throw new IllegalStateException("Illegal assignment identified")
        vars(variable) = left
        println(variable + ": " + left)
        return left
    }

    exps.foreach(exp =>
        this.value(exp.result)
    )
}
