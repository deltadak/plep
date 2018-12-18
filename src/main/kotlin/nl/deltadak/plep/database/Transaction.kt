package nl.deltadak.plep.database

import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.Transaction
import java.sql.Connection
import org.jetbrains.exposed.sql.transactions.transaction
import java.io.File

/**
 * Use instead of [transaction], as this sets default flags to work with SQLite.
 * Also sets the path of the database, which should be stored next to executable/jar file.
 */
fun <T> regularTransaction(statement: Transaction.() -> T): T {
    val databasePath = "jdbc:sqlite:${File(DatabaseFacade::class.java.protectionDomain.codeSource.location.toURI()).parent}/plep.db"
    Database.connect(databasePath, driver = "org.sqlite.JDBC") // Database for actual application.
//    Database.connect("jdbc:sqlite:file:test", driver = "org.sqlite.JDBC") // Database for testing.

    return transaction(Connection.TRANSACTION_SERIALIZABLE, 2, statement = statement)
}
