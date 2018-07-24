/**
 * Script to generate the similar data to experiment 1
 */
@Grab(group='org.apache.commons', module='commons-csv', version='1.5')
@Grab(group='com.google.guava', module='guava', version='21.0')
@Grab(group='org.apache.commons', module='commons-math3', version='3.6.1')

import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter
import org.apache.commons.math3.distribution.GeometricDistribution

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.security.SecureRandom

def patternFamilies = [ "Creational" : ["Factory Method", "Singleton", "Prototype"],
                       "Structural" : ["(Object) Adapter", "Bridge", "Composite", "Decorator", "Proxy"],
                       "Behavioral" : ["Chain of Responsibility", "Command", "Observer", "State", "Strategy", "Template Method", "Visitor"]]
Random rand = new SecureRandom()
GeometricDistribution distGS = new GeometricDistribution(0.8d)
GeometricDistribution distPS = new GeometricDistribution(0.8d)
Path p = Paths.get("test_data_cs2.csv")
final Appendable out = Files.newBufferedWriter(p, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
final CSVPrinter printer = CSVFormat.DEFAULT.withHeader("InstID", "ChainID", "GSZ", "PS", "PT", "PF").print(out)

def instID = 0
patternFamilies.each { pf, types ->
    types.each { pt ->
        (1..3).each { chainID ->
            grimeSize = rand.nextInt(4) + 1
            patternSize = rand.nextInt(4) + 1
            (1..10).each {
                instID++
                grimeSize += nextNum(distGS, rand)
                patternSize += nextNum(distPS, rand)

                Object[] record = [
                        instID,
                        chainID,
                        grimeSize,
                        patternSize,
                        pt,
                        pf
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
}
printer.close()

static def nextNum(GeometricDistribution dist, Random rand) {
    double sample = dist.sample()
    if (rand.nextBoolean())
        sample = 0

    return sample
}