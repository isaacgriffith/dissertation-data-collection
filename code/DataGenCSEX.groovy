/**
 * Script to generate the similar data to experiment 1
 */
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter

@Grab(group='org.apache.commons', module='commons-csv', version='1.5')
@Grab(group='com.google.guava', module='guava', version='21.0')
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.security.SecureRandom

Random rand = new SecureRandom()
Path p = Paths.get("test_data_csex.csv")
final Appendable out = Files.newBufferedWriter(p, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
final CSVPrinter printer = CSVFormat.DEFAULT.withHeader("ID", "FunctionalSuitability", "Maintainability", "PerformanceEfficiency", "Reliability", "Security", "TechDebt").print(out)

(1..500).each { currentID ->
    Object[] record = [
            currentID,
            rand.nextDouble() - 0.5,
            rand.nextDouble() - 0.5,
            rand.nextDouble() - 0.5,
            rand.nextDouble() - 0.5,
            rand.nextDouble() - 0.5,
            rand.nextDouble() - 0.5
    ]
    // print record
    try {
        printer.printRecord(record)
    } catch (IOException e) {
        e.printStackTrace()
    }
}
printer.close()
