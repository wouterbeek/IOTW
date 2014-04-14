package com.wouterbeek.iotw;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.Map;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.reasoner.Reasoner;
import com.hp.hpl.jena.reasoner.ReasonerRegistry;
import com.hp.hpl.jena.util.FileManager;

public class Main {
  // Define a static logger variable that references this class.
  public static Logger logger = Logger.getLogger(Main.class);

  public static void main(String args[]) {
    // Set up a simple configuration that logs on the console.
    BasicConfigurator.configure();

    if (args.length == 0) {
      run_ui();
    } else {
      run_for_files(new File(args[0]).listFiles(), new File(args[1]));
    }
  }
  
  public static void run_ui() {
    // Open the file chooser for opening files.
    final JFileChooser fc = new JFileChooser();

    // If a directory is selected, then all files in that directory
    // are loaded (this behavior is performed recursively
    // for sub-directories as well).
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    int returnValue = fc.showOpenDialog(null);
    if (returnValue == JFileChooser.APPROVE_OPTION) {
      File input_file = fc.getSelectedFile();
      run_for_files(input_file.listFiles(), new File(
          input_file.getParentFile(), "stage3"));
    }
  }

  private static void run_for_files(File[] input_files, File output_dir) {
    for (File input_file : input_files) {
      run_for_file(input_file, output_dir);
    }
  }

  private static void run_for_file(File input_file, File output_dir) {
    if (input_file.isDirectory()) {
      run_for_files(input_file.listFiles(),
          new File(output_dir, input_file.getName()));
    } else if (isSupportedFile(input_file)) {
      run_for_single_file(input_file, output_dir);
    }
  }

  private static boolean isSupportedFile(File file) {
    return new FileNameExtensionFilter("RDF serializations", "rdf", "ttl",
        "owl").accept(file);
  }

  private static void run_for_single_file(File input_file, File output_dir) {
    // Make sure the output directory exists.
    output_dir.mkdirs();

    InfModel inference_model = materialize_model(FileManager.get().loadModel(
        input_file.getAbsolutePath()));

    // DEB
    // print_model(inference_model);

    try {
      inference_model.write(new FileOutputStream(new File(output_dir,
          input_file.getName())), "TURTLE");
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }
  }

  private static void print_model(Model model) {
    // list the statements in the Model
    StmtIterator iter = model.listStatements();

    // print out the predicate, subject and object of each statement
    int index = 0;
    while (iter.hasNext()) {
      Statement stmt = iter.nextStatement(); // get next statement
      Resource subject = stmt.getSubject();
      Property predicate = stmt.getPredicate();
      RDFNode object = stmt.getObject();
      System.out.println("[" + index + "] " + print_resource(subject) + "\t"
          + print_resource(predicate) + "\t" + print_resource(object));
      index++;
    }
  }

  private static String print_resource(RDFNode node) {
    if (node.isURIResource()) {
      return print_uri_resource((Resource) node);
    } else if (node.isLiteral()) {
      return print_literal((Literal) node);
    } else if (node.isAnon()) {
      return node.toString();
    } else {
      return "";
    }
  }

  private static String print_uri_resource(Resource resource) {
    String namespace = resource.getModel().getNsURIPrefix(
        resource.getNameSpace());
    return namespace + ":" + resource.getLocalName();
  }

  private static String print_literal(Literal literal) {
    String tmp;
    if (literal.getDatatype() != null) {
      tmp = literal.getLexicalForm() + "\"^^" + literal.getDatatype().getURI();
    } else if (literal.getLanguage() != "") {
      tmp = literal.getString() + "\"@" + literal.getLanguage();
    } else {
      tmp = literal.getString();
    }
    return "\"" + tmp + "\"";
  }

  private static InfModel materialize_model(Model model) {
    Reasoner reasoner = ReasonerRegistry.getOWLReasoner();
    return ModelFactory.createInfModel(reasoner, model);
  }

  private static void print_namespaces(Model model) {
    for (Map.Entry<String, String> entry : model.getNsPrefixMap().entrySet()) {
      System.out.println("Key:" + entry.getKey() + "\tValue:"
          + entry.getValue());
    }
  }
}
