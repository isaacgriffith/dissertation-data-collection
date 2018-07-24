/**
 * Script to generate the similar data to experiment 1
 */
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter
import org.apache.commons.math3.distribution.BetaDistribution

@Grab(group='org.apache.commons', module='commons-csv', version='1.5')
@Grab(group='com.google.guava', module='guava', version='21.0')
@Grab(group='org.apache.commons', module='commons-math3', version='3.6.1')
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.security.SecureRandom

def grimeTypes = ["PIG", "TIG", "PEAG", "PEEG", "TEAG", "TEEG",
                  "DIPG", "DISG", "DEPG", "DESG", "IIPG", "IISG", "IEPG", "IESG",
                  "PICG", "PIRG", "PECG", "PERG",
                  "MPICG", "MPIUG", "MPECG", "MPEUG", "MTICG", "MTIUG", "MTECG", "MTEUG"]
def patternTypes = ["Factory Method", "Prototype", "Singleton", "(Object)Adapter",
                    "Command", "Composite", "Decorator", "Observer",
                    "State", "Strategy", "Bridge", "Template Method",
                    "Visitor", "Proxy", "Chain of Responsibility"]
def severity = 1..6

def currentID = 0
Random rand = new SecureRandom()
Path p = Paths.get("test_data_ex2.csv")
final Appendable out = Files.newBufferedWriter(p, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
final CSVPrinter printer = CSVFormat.DEFAULT.withHeader("ID", "PT", "GT", "GS", "TechnicalDebt_P").print(out)
BetaDistribution dist = new BetaDistribution(2, 5)

patternTypes.each { pt ->
    grimeTypes.each { gt ->
        severity.each { gs ->
            currentID += 1
            Object[] record = [
                    currentID,
                    pt,
                    gt,
                    gs,
                    nextNum(dist, rand),
            ]
            // print record
            try {
                printer.printRecord(record)
            } catch (IOException e) {
                e.printStackTrace()
            }
        }
    }
}
printer.close()

static def nextNum(BetaDistribution dist, Random rand) {
    double sample = dist.sample()
    if (rand.nextBoolean())
        sample *= -1

    return sample
}
