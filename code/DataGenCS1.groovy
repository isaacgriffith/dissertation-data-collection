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

String[] header = ["InstChainID", "PIG", "TIG", "PEAG", "PEEG", "TEAG", "TEEG",
                   "DIPG", "DISG", "DEPG", "DESG", "IIPG", "IISG", "IEPG", "IESG",
                   "PICG", "PIRG", "PECG", "PERG",
                   "MPICG", "MPIUG", "MPECG", "MPEUG", "MTICG", "MTIUG", "MTECG", "MTEUG"]
Random rand = new SecureRandom()
Path p = Paths.get("test_data_cs1.csv")
final Appendable out = Files.newBufferedWriter(p, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
final CSVPrinter printer = CSVFormat.DEFAULT.withHeader(header).print(out)

1.upto(5000) { chainID ->
    def record = [chainID.toString()]
    (1..26).each {
        def value = 0
        if (rand.nextBoolean())
            value = 1
        record << value
    }
    // print record
    try {
        printer.printRecord(record)
    } catch (IOException e) {
        e.printStackTrace()
    }
}
printer.close()
