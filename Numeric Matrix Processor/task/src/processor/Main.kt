package processor

class MatrixInverseException (message: String): Exception(message)
data class MenuItem (val code: String, val num: Int, val name: String = "")

abstract class AMenu {
    open val items: MutableList<MenuItem> = mutableListOf()
    fun print() {
        items.filter { it.code != "UNKNOWN" }.forEach { println("${it.num}. ${it.name}") }
    }

    fun getCodeByNumber(n: Int): String {
        val filtered = items.filter { it.num == n }
        if (filtered.size == 1)
            return filtered[0].code
        return "UNKNOWN"
    }
}
object MainMenu: AMenu() {
    override val items = mutableListOf(
        MenuItem("SUM", 1, "Add matrices"),
        MenuItem("MULT_C", 2, "Multiply matrix by a constant"),
        MenuItem("MUL", 3, "Multiply matrices"),
        MenuItem("TRANSPOSE", 4, "Transpose matrix"),
        MenuItem("DETERMINANT", 5, "Calculate a determinant"),
        MenuItem("INVERSE", 6, "Inverse matrix"),
        MenuItem("EXIT", 0, "Exit"),
    )
}
object TransposeMenu: AMenu() {
    override val items = mutableListOf(
        MenuItem(Matrix.Transpose.MAIN_DIAGONAL.name, 1, "Main diagonal"),
        MenuItem(Matrix.Transpose.SIDE_DIAGONAL.name, 2, "Side diagonal"),
        MenuItem(Matrix.Transpose.VERTICAL_LINE.name, 3, "Vertical line"),
        MenuItem(Matrix.Transpose.HORIZONTAL_LINE.name, 4, "Horizontal line"),
    )
}

class Matrix  (val rows:Int, val cols: Int)  {
    enum class Transpose {MAIN_DIAGONAL, SIDE_DIAGONAL, VERTICAL_LINE, HORIZONTAL_LINE}
    private val table: MutableList<MutableList<Double>> = MutableList(rows) { MutableList(cols) {0.0}}

    fun generateNewMatrix (rows: Int, cols: Int, generateFunction: (Int, Int) -> Double): Matrix {
        val result= Matrix(rows, cols)
        for (row in 1..rows) {
            for (col in 1..cols) {
                result.set(row, col, generateFunction(row, col))
            }
        }
        return result
    }

    fun set(row: Int, col: Int, value: Double) {
        check (row in 1..rows) {"Wrong row. Presented is $row, allowed 1..$rows"}
        check (col in 1..cols) {"Wrong col. Presented is $col, allowed 1..$cols"}
        table[row-1][col-1] = value
    }

    fun get(row: Int, col: Int): Double {
        check (row in 1..rows) {"Wrong row. Presented is $row, allowed 1..$rows"}
        check (col in 1..cols) {"Wrong col. Presented is $col, allowed 1..$cols"}
        return table[row-1][col-1]
    }

    operator fun plus (m: Matrix):Matrix {
        check (rows == m.rows && cols == m.cols) {"Wrong matrix size"}
        return generateNewMatrix(rows, cols) { row, col -> get(row,col) + m.get(row,col) }
    }

    override fun toString():String {
        var s = ""
        table.forEach { s += it.joinToString( separator = " ", transform = { it.format()})+"\n" }
        return s
    }

    operator fun times(constant: Double):Matrix {
        return generateNewMatrix(rows, cols) { row, col -> get(row, col) * constant }
    }

    private fun getRowVector(row: Int):MutableList<Double> {
        check (row in 1..rows) {"Wrong row. Presented is $row, allowed 1..$rows"}
        return table[row-1]
    }

    private fun getColVector(col: Int): MutableList<Double> {
        check (col in 1..cols) {"Wrong col. Presented is $col, allowed 1..$cols"}
        val result = mutableListOf<Double>()
        for (row in 1..(rows)) {
            result.add(get(row,col))
        }
        return result
    }


    operator fun times(m: Matrix):Matrix {
        check(cols == m.rows) {"The operation cannot be performed."}
        return generateNewMatrix(rows, m.cols, fun(row: Int, col: Int): Double {
            var value = 0.0
            val rowVector = this.getRowVector(row)
            val colVector = m.getColVector(col)
            for (i in rowVector.indices) {
                value += rowVector[i] * colVector[i]
            }
            return value
        })
    }
    private fun subMatrix (excludeRow: Int, excludeCol: Int): Matrix {
        return generateNewMatrix(rows - 1, cols - 1) {row, col -> get(if(row < excludeRow) row else row+1, if(col < excludeCol) col else col+1) }
    }

    private fun cofactor (row: Int, col: Int): Double {
        return subMatrix(row,col).determinant() * Math.pow(-1.0, (row + col).toDouble())
    }

    fun determinant():Double {
        check(rows == cols) {"For Determinant it should be a square matrix"}
        if (rows == 1)
            return get (1,1)

        var s = 0.0
        for (col in 1..cols)
            s += get(1,col) * cofactor(1, col)

        return s
    }

    fun transpose(type: Transpose = Transpose.MAIN_DIAGONAL):Matrix {
        return when (type) {
            Transpose.HORIZONTAL_LINE ->  generateNewMatrix(rows, cols) { row, col -> get(rows + 1 - row,col) }
            Transpose.VERTICAL_LINE   ->  generateNewMatrix(rows, cols) { row, col -> get(row,cols + 1 - col) }
            Transpose.MAIN_DIAGONAL   ->  generateNewMatrix(rows, cols) { row, col -> get(col,row) }
            Transpose.SIDE_DIAGONAL   ->  generateNewMatrix(rows, cols) { row, col -> get(cols + 1 - col, rows + 1 - row) }
        }
    }

    fun inverse(): Matrix {
        val det = determinant()
        val c = generateNewMatrix(rows, cols) { row, col -> cofactor(row, col)}
        if (det == 0.0) throw MatrixInverseException("This matrix doesn't have an inverse.")
        return  c.transpose() * (1 / det)
    }
}


fun Double.format() = if("%.2f".format(this).contains(".00")) "%.0f".format(this) else "%.2f".format(this)
fun getMatrixFromInput(matrixOrder: String = ""):Matrix {

    print("Enter size of $matrixOrder matrix:".replace("  ", " "))
    val (rows, cols) = readln().trim().split(" ").map {it.toInt()}
    println("Enter $matrixOrder matrix:")
    val matrix = Matrix(rows, cols)
    for (row in 1..rows) {
        val arrRow = readln().trim().split(" ").map {it.toDouble()}

        check(arrRow.size == cols) {"Too many inputs in row for matrix $rows x $cols"}
        for (col in 1..arrRow.size) {
            matrix.set(row, col, arrRow[col-1])
        }
    }
    return matrix
}

fun printResult (result: Any) {
    println("The result is:")
    println(result)
}

fun main() {

    do {
        MainMenu.print()
        print("Your choice: ")
        val action = MainMenu.getCodeByNumber(readln().toInt())
        try {
            when (action) {
                "SUM" -> printResult(getMatrixFromInput("first") + getMatrixFromInput("second"))
                "MULT_C" -> {
                    val m1 = getMatrixFromInput()
                    print("Enter constant: ")
                    val c = readln().toDouble()
                    printResult(m1 * c)
                }
                "MUL" -> printResult(getMatrixFromInput("first") * getMatrixFromInput("second"))
                "TRANSPOSE" -> {
                    TransposeMenu.print()
                    print("Your choice: ")
                    val code = TransposeMenu.getCodeByNumber(readln().trim().toInt())
                    if (code != "UNKNOWN") {
                        printResult(getMatrixFromInput().transpose(Matrix.Transpose.valueOf(code)))
                    }
                }
                "DETERMINANT" -> printResult(getMatrixFromInput().determinant().format())
                "INVERSE" -> printResult(getMatrixFromInput().inverse())
            }
        } catch (e: MatrixInverseException) { println(e.message + "\n") }
          catch (e: Exception) { println("The operation cannot be performed.\n") }
    } while (action != "EXIT")

}
