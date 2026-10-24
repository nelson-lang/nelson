//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenMainHarness.hpp"
#include "NFlowVersion.hpp"
//=============================================================================
namespace nflow {
//=============================================================================
std::vector<std::string>
emitCMainLines(const CMainSpec& s)
{
    std::vector<std::string> main_lines;
    main_lines.push_back("#include \"model.c\"");
    main_lines.push_back("");
    main_lines.push_back("typedef struct {");
    main_lines.push_back("  double* time;");
    main_lines.push_back("  double* values;");
    main_lines.push_back("  int count;");
    main_lines.push_back("  int capacity;");
    main_lines.push_back("} InputSeries;");
    main_lines.push_back("");
    main_lines.push_back("static void init_series(InputSeries* s) {");
    main_lines.push_back("  s->time = NULL;");
    main_lines.push_back("  s->values = NULL;");
    main_lines.push_back("  s->count = 0;");
    main_lines.push_back("  s->capacity = 0;");
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("static void free_series(InputSeries* s) {");
    main_lines.push_back("  free(s->time);");
    main_lines.push_back("  free(s->values);");
    main_lines.push_back("  s->time = NULL;");
    main_lines.push_back("  s->values = NULL;");
    main_lines.push_back("  s->count = 0;");
    main_lines.push_back("  s->capacity = 0;");
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("static int ensure_capacity(InputSeries* s, int needed) {");
    main_lines.push_back("  if (needed <= s->capacity) return 1;");
    main_lines.push_back("  int newCap = s->capacity ? s->capacity * 2 : 256;");
    main_lines.push_back("  while (newCap < needed) newCap *= 2;");
    main_lines.push_back("  double* newTime = (double*)realloc(s->time, sizeof(double) * newCap);");
    main_lines.push_back("  if (!newTime) return 0;");
    main_lines.push_back(
        "  double* newValues = (double*)realloc(s->values, sizeof(double) * newCap "
        "* (INPUT_COUNT > 0 ? INPUT_COUNT : 1));");
    main_lines.push_back("  if (!newValues) return 0;");
    main_lines.push_back("  s->time = newTime;");
    main_lines.push_back("  s->values = newValues;");
    main_lines.push_back("  s->capacity = newCap;");
    main_lines.push_back("  return 1;");
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("static int read_csv(const char* path, InputSeries* series) {");
    main_lines.push_back("  if (!path) return 0;");
    main_lines.push_back("  FILE* f = fopen(path, \"r\");");
    main_lines.push_back("  if (!f) return 0;");
    main_lines.push_back("  char line[4096];");
    main_lines.push_back("  if (!fgets(line, sizeof(line), f)) { fclose(f); return 0; }");
    main_lines.push_back("  int colMap[128];");
    main_lines.push_back("  int colCount = 0;");
    main_lines.push_back("  char* token = strtok(line, \",\\n\\r\");");
    main_lines.push_back("  while (token && colCount < 128) {");
    main_lines.push_back("    if (strcmp(token, \"t\") == 0 || strcmp(token, \"time\") == 0) "
                         "colMap[colCount] = -1;");
    main_lines.push_back("    else {");
    main_lines.push_back("      int idx = -1;");
    main_lines.push_back("      for (int i = 0; i < INPUT_COUNT; i++) {");
    main_lines.push_back("        if (strcmp(token, input_names[i]) == 0) { idx = i; break; }");
    main_lines.push_back("      }");
    main_lines.push_back("      colMap[colCount] = idx;");
    main_lines.push_back("    }");
    main_lines.push_back("    colCount++;");
    main_lines.push_back("    token = strtok(NULL, \",\\n\\r\");");
    main_lines.push_back("  }");
    main_lines.push_back("  while (fgets(line, sizeof(line), f)) {");
    main_lines.push_back("    if (!ensure_capacity(series, series->count + 1)) break;");
    main_lines.push_back("    double t = 0.0;");
    main_lines.push_back("    for (int i = 0; i < INPUT_COUNT; i++) {");
    main_lines.push_back(
        "      series->values[series->count * (INPUT_COUNT > 0 ? INPUT_COUNT : 1) + i] = 0.0;");
    main_lines.push_back("    }");
    main_lines.push_back("    int col = 0;");
    main_lines.push_back("    token = strtok(line, \",\\n\\r\");");
    main_lines.push_back("    while (token && col < colCount) {");
    main_lines.push_back("      double v = strtod(token, NULL);");
    main_lines.push_back("      if (colMap[col] == -1) t = v;");
    main_lines.push_back("      else if (colMap[col] >= 0) series->values[series->count * "
                         "(INPUT_COUNT > 0 ? INPUT_COUNT : 1) + colMap[col]] = v;");
    main_lines.push_back("      col++;");
    main_lines.push_back("      token = strtok(NULL, \",\\n\\r\");");
    main_lines.push_back("    }");
    main_lines.push_back("    series->time[series->count] = t;");
    main_lines.push_back("    series->count += 1;");
    main_lines.push_back("  }");
    main_lines.push_back("  fclose(f);");
    main_lines.push_back("  return series->count;");
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("static void fill_inputs(const InputSeries* series, int* idx, double "
                         "t, ModelInput* in) {");
    main_lines.push_back("  if (!series || series->count == 0 || INPUT_COUNT == 0) return;");
    main_lines.push_back("  while (*idx + 1 < series->count && series->time[*idx + 1] <= t) {");
    main_lines.push_back("    *idx += 1;");
    main_lines.push_back("  }");
    main_lines.push_back("  int base = (*idx) * INPUT_COUNT;");
    if (s.anyTypedInput || s.hasBusInputs) {
        // Mixed-type or struct-typed ModelInput: the flat double cast is
        // invalid, so assign each column to its own field.
        for (size_t i = 0; i < s.inColumns.size(); ++i) {
            const auto& col = s.inColumns[i];
            std::string cast;
            if (col.typeName == "int64_t") {
                cast = "(int64_t)";
            } else if (col.typeName == "uint64_t") {
                cast = "(uint64_t)";
            }
            main_lines.push_back("  in->" + col.label + " = " + cast + "series->values[base + "
                + std::to_string(i) + "];");
        }
    } else {
        main_lines.push_back("  for (int i = 0; i < INPUT_COUNT; i++) {");
        main_lines.push_back("    ((double*)in)[i] = series->values[base + i];");
        main_lines.push_back("  }");
    }
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("static void write_header(FILE* f) {");
    main_lines.push_back("  fprintf(f, \"t\");");
    main_lines.push_back(
        "  for (int i = 0; i < OUTPUT_COUNT; i++) fprintf(f, \",%s\", output_names[i]);");
    main_lines.push_back("  fprintf(f, \"\\n\");");
    main_lines.push_back("}");
    main_lines.push_back("");
    main_lines.push_back("int main(int argc, char** argv) {");
    main_lines.push_back("  double tEnd = " + s.durationLiteral + ";");
    main_lines.push_back("  const char* inPath = NULL;");
    main_lines.push_back("  const char* outPath = NULL;");
    main_lines.push_back("  const double dt = " + s.dtLiteral + ";");
    main_lines.push_back("  for (int i = 1; i < argc; i++) {");
    main_lines.push_back(
        "    if (strcmp(argv[i], \"-t\") == 0 && i + 1 < argc) tEnd = atof(argv[++i]);");
    main_lines.push_back(
        "    else if (strcmp(argv[i], \"-i\") == 0 && i + 1 < argc) inPath = argv[++i];");
    main_lines.push_back(
        "    else if (strcmp(argv[i], \"-o\") == 0 && i + 1 < argc) outPath = argv[++i];");
    main_lines.push_back("  }");
    main_lines.push_back("  ModelState state;");
    main_lines.push_back("  ModelInput in = {0};");
    main_lines.push_back("  ModelOutput out = {0};");
    main_lines.push_back("  nflow_init(&state);");
    main_lines.push_back("  InputSeries series;");
    main_lines.push_back("  init_series(&series);");
    main_lines.push_back("  int seriesIdx = 0;");
    main_lines.push_back("  if (inPath) read_csv(inPath, &series);");
    main_lines.push_back("  FILE* outFile = outPath ? fopen(outPath, \"w\") : stdout;");
    main_lines.push_back("  if (!outFile) outFile = stdout;");
    main_lines.push_back("  write_header(outFile);");
    // Sample instants are k*dt, not an accumulated sum: adding dt drifts (eight
    // additions of 0.05 land just BELOW 0.4) and moves a sample across a boundary
    // that a pulse, step or reset trigger is keyed on. Same as the Rust runner
    // and the simulator.
    main_lines.push_back("  const long __nsteps = (long)(tEnd / dt) + 1;");
    main_lines.push_back("  for (long __k = 0; __k <= __nsteps; ++__k) {");
    main_lines.push_back("    const double t = __k * dt;");
    main_lines.push_back("    if (t > tEnd + 1e-9) break;");
    main_lines.push_back("    if (inPath) fill_inputs(&series, &seriesIdx, t, &in);");
    main_lines.push_back("    nflow_step(&state, &in, &out, t);");
    main_lines.push_back("    fprintf(outFile, \"%.17g\", t);");
    if (s.anyTypedOutput || s.hasBusOutputs) {
        // Mixed-type or struct-typed ModelOutput: the flat double cast is
        // invalid, so print each column with its own format.
        for (const auto& col : s.outColumns) {
            if (col.typeName == "int64_t") {
                main_lines.push_back(
                    "    fprintf(outFile, \",%lld\", (long long)out." + col.label + ");");
            } else if (col.typeName == "uint64_t") {
                main_lines.push_back(
                    "    fprintf(outFile, \",%llu\", (unsigned long long)out." + col.label + ");");
            } else {
                main_lines.push_back("    fprintf(outFile, \",%.17g\", out." + col.label + ");");
            }
        }
    } else {
        main_lines.push_back("    for (int i = 0; i < OUTPUT_COUNT; i++) {");
        main_lines.push_back("      fprintf(outFile, \",%.17g\", ((double*)&out)[i]);");
        main_lines.push_back("    }");
    }
    main_lines.push_back("    fprintf(outFile, \"\\n\");");
    if (s.hasStopBlock) {
        // stopSimulation: the stopping step's row is written, then the run ends.
        main_lines.push_back("    if (state.nflow_stop_requested != 0.0) break;");
    }
    main_lines.push_back("  }");
    main_lines.push_back("  if (outPath && outFile && outFile != stdout) fclose(outFile);");
    main_lines.push_back("  free_series(&series);");
    main_lines.push_back("  return 0;");
    main_lines.push_back("}");
    main_lines.push_back("");
    return main_lines;
}
//=============================================================================
std::vector<std::string>
emitRustMainLines(const RustMainSpec& s)
{
    std::vector<std::string> m;

    m.push_back("// Generated by NFlow " NFLOW_PRODUCT_VERSION " - host runner (uses std)");
    m.push_back("// Links against the no_std `nflow_model` lib.");
    m.push_back("use nflow_model::{ModelInput, ModelOutput, ModelState,");
    m.push_back("                  nflow_init, nflow_step,");
    m.push_back("                  INPUT_COUNT, OUTPUT_COUNT};");
    m.push_back("use std::fs::File;");
    m.push_back("use std::io::{BufRead, BufReader, Write, BufWriter};");
    m.push_back("");

    m.push_back("/// Map CSV header tokens to ModelInput field indices.");
    m.push_back("fn parse_header(header: &str) -> Vec<Option<usize>> {");
    m.push_back("    let input_names: [&str; INPUT_COUNT] = [");
    for (const auto& col : s.inColumns) {
        m.push_back("        \"" + col.label + "\",");
    }
    if (s.inColumns.empty()) {
        m.push_back("        // (no external inputs)");
    }
    m.push_back("    ];");
    m.push_back("    header.trim_end_matches(|c| c == '\\r' || c == '\\n')");
    m.push_back("          .split(',')");
    m.push_back("          .map(|tok| {");
    m.push_back("              if tok == \"t\" || tok == \"time\" { return None; }");
    m.push_back("              input_names.iter().position(|&n| n == tok)");
    m.push_back("          })");
    m.push_back("          .collect()");
    m.push_back("}");
    m.push_back("");

    // Row width is known at generation time - avoid INPUT_COUNT.max(1)
    // which is not valid in a const context on stable Rust.
    const size_t rowWidth = (s.inColumns.size() > 0) ? s.inColumns.size() : 1;
    const std::string rowWidthStr = std::to_string(rowWidth);

    m.push_back("struct InputSeries {");
    m.push_back("    times:  Vec<f64>,");
    m.push_back("    values: Vec<[f64; " + rowWidthStr + "]>,");
    m.push_back("}");
    m.push_back("");

    m.push_back("fn read_csv(path: &str) -> InputSeries {");
    m.push_back("    let mut series = InputSeries { times: vec![], values: vec![] };");
    m.push_back("    let Ok(file) = File::open(path) else { return series; };");
    m.push_back("    let mut lines = BufReader::new(file).lines();");
    m.push_back("    let Some(Ok(hdr)) = lines.next() else { return series; };");
    m.push_back("    let col_map = parse_header(&hdr);");
    m.push_back("    for line in lines.flatten() {");
    m.push_back("        let mut t = 0.0_f64;");
    m.push_back("        let mut row = [0.0_f64; " + rowWidthStr + "];");
    m.push_back("        for (col, tok) in line.split(',').enumerate() {");
    m.push_back("            let v: f64 = tok.trim().parse().unwrap_or(0.0);");
    m.push_back("            match col_map.get(col) {");
    m.push_back("                Some(None)      => t = v,");
    // idx is already usize, not &usize: no & in the pattern
    m.push_back("                Some(Some(idx)) => row[*idx] = v,");
    m.push_back("                None            => {}");
    m.push_back("            }");
    m.push_back("        }");
    m.push_back("        series.times.push(t);");
    m.push_back("        series.values.push(row);");
    m.push_back("    }");
    m.push_back("    series");
    m.push_back("}");
    m.push_back("");

    // Prefix with _ when no inputs to silence unused-variable warnings.
    const std::string inpParam = s.inColumns.empty() ? "_inp" : "inp";
    const std::string rowVar = s.inColumns.empty() ? "_row" : "row";
    m.push_back("fn fill_inputs(series: &InputSeries, idx: &mut usize,");
    m.push_back("               t: f64, " + inpParam + ": &mut ModelInput) {");
    m.push_back("    if series.times.is_empty() || INPUT_COUNT == 0 { return; }");
    m.push_back("    while *idx + 1 < series.times.len()");
    m.push_back("          && series.times[*idx + 1] <= t { *idx += 1; }");
    m.push_back("    let " + rowVar + " = &series.values[*idx];");
    for (size_t i = 0; i < s.inColumns.size(); ++i) {
        const auto& col = s.inColumns[i];
        std::string cast;
        if (col.typeName == "i64") {
            cast = " as i64";
        } else if (col.typeName == "u64") {
            cast = " as u64";
        }
        m.push_back("    inp." + col.label + " = row[" + std::to_string(i) + "]" + cast + ";");
    }
    m.push_back("}");
    m.push_back("");

    // ?Sized allows passing &mut dyn Write (trait object) through Box<dyn Write>
    m.push_back("fn write_header(w: &mut (impl Write + ?Sized)) {");
    m.push_back("    let output_names: [&str; OUTPUT_COUNT] = [");
    for (const auto& col : s.outColumns) {
        m.push_back("        \"" + col.label + "\",");
    }
    if (s.outColumns.empty()) {
        m.push_back("        // (no external outputs)");
    }
    m.push_back("    ];");
    m.push_back("    write!(w, \"t\").unwrap();");
    m.push_back("    for name in &output_names { write!(w, \",{}\", name).unwrap(); }");
    m.push_back("    writeln!(w).unwrap();");
    m.push_back("}");
    m.push_back("");

    // ?Sized as in write_header. Prefix `out` with _ when there are no output
    // labels, to avoid the unused-variable warning on diagrams with no label sinks.
    const std::string outParam = s.outputNames.empty() ? "_out" : "out";
    m.push_back(
        "fn write_row(w: &mut (impl Write + ?Sized), t: f64, " + outParam + ": &ModelOutput) {");
    m.push_back("    write!(w, \"{}\", t).unwrap();");
    if (s.hasBusOutputs) {
        // Struct-typed outputs: one column per packed element, integer
        // lanes printed without a float precision.
        for (const auto& col : s.outColumns) {
            m.push_back("    write!(w, \",{}\", out." + col.label + ").unwrap();");
        }
    } else {
        for (const auto& nm : s.outputNames) {
            m.push_back("    write!(w, \",{}\", out." + nm + ").unwrap();");
        }
    }
    m.push_back("    writeln!(w).unwrap();");
    m.push_back("}");
    m.push_back("");

    m.push_back("fn main() {");
    m.push_back("    // Usage:");
    m.push_back("    //   nflow_run [-t <duration>] [-i <input.csv>] [-o <output.csv>]");
    m.push_back("    //");
    m.push_back("    //   -t <duration>   Simulation end time in seconds.");
    m.push_back("    //                   Default: " + s.durationLiteral
        + "  (taken from diagram runtime)");
    m.push_back("    //");
    m.push_back("    //   -i <input.csv>  Optional CSV file supplying external input signals.");
    m.push_back("    //                   The first row must be a header with column names.");
    m.push_back("    //                   One column must be named \"t\" or \"time\" for");
    m.push_back("    //                   the time axis; remaining columns are matched by");
    m.push_back("    //                   name to the model input labels listed below.");
    m.push_back("    //                   Rows are applied with zero-order hold between");
    m.push_back("    //                   sample points.");
    if (!s.inColumns.empty()) {
        m.push_back("    //                   Expected input labels:");
        for (const auto& col : s.inColumns) {
            m.push_back("    //                     - " + col.label);
        }
    } else {
        m.push_back("    //                   (This model has no external input labels.)");
    }
    m.push_back("    //");
    m.push_back("    //   -o <output.csv> CSV file to write simulation results to.");
    m.push_back("    //                   If omitted, results are written to stdout.");
    m.push_back("    //                   The header row lists \"t\" followed by the");
    m.push_back("    //                   model output labels listed below.");
    if (!s.outputNames.empty()) {
        m.push_back("    //                   Output labels:");
        for (const auto& nm : s.outputNames) {
            m.push_back("    //                     - " + nm);
        }
    } else {
        m.push_back("    //                   (This model has no external output labels.)");
    }
    m.push_back("    //");
    m.push_back("    //   Sample time (dt): " + s.dtLiteral);
    m.push_back("    let args: Vec<String> = std::env::args().collect();");
    m.push_back("    let mut t_end:   f64            = " + s.durationLiteral + ";");
    m.push_back("    let mut in_path: Option<String> = None;");
    m.push_back("    let mut out_path:Option<String> = None;");
    m.push_back("    let dt: f64 = " + s.dtLiteral + ";");
    m.push_back("");
    m.push_back("    let mut i = 1;");
    m.push_back("    while i < args.len() {");
    m.push_back("        match args[i].as_str() {");
    m.push_back("            \"-t\" if i + 1 < args.len() => {");
    m.push_back("                // Override simulation end time");
    m.push_back("                t_end = args[i+1].parse().unwrap_or(t_end); i += 2; }");
    m.push_back("            \"-i\" if i + 1 < args.len() => {");
    m.push_back("                // Path to input CSV");
    m.push_back("                in_path = Some(args[i+1].clone()); i += 2; }");
    m.push_back("            \"-o\" if i + 1 < args.len() => {");
    m.push_back("                // Path to output CSV (default: stdout)");
    m.push_back("                out_path = Some(args[i+1].clone()); i += 2; }");
    m.push_back("            _ => { i += 1; }");
    m.push_back("        }");
    m.push_back("    }");
    m.push_back("");
    m.push_back("    let series = in_path.as_deref().map(read_csv)");
    m.push_back(
        "                         .unwrap_or(InputSeries { times: vec![], values: vec![] });");
    m.push_back("    let mut series_idx: usize = 0;");
    m.push_back("");
    m.push_back("    // Simulation state");
    m.push_back("    let mut state  = ModelState::default();");
    m.push_back("    nflow_init(&mut state);");
    m.push_back("    let mut inp    = ModelInput::default();");
    m.push_back("    let mut out    = ModelOutput::default();");
    m.push_back("");
    m.push_back("    // Output writer - file or stdout");
    m.push_back("    let stdout   = std::io::stdout();");
    m.push_back("    let out_file: Box<dyn Write> = match out_path.as_deref() {");
    m.push_back("        Some(p) => Box::new(BufWriter::new(");
    m.push_back("                       File::create(p).expect(\"cannot create output file\"))),");
    m.push_back("        None    => Box::new(BufWriter::new(stdout.lock())),");
    m.push_back("    };");
    m.push_back("    let mut w = out_file;");
    m.push_back("    write_header(&mut *w);");
    m.push_back("");
    m.push_back("    // Simulation loop");
    m.push_back("    let steps = ((t_end / dt) as usize) + 1;");
    m.push_back("    for step in 0..=steps {");
    m.push_back("        let t = step as f64 * dt;");
    m.push_back("        if t > t_end + 1e-9 { break; }");
    m.push_back("        if in_path.is_some() {");
    m.push_back("            fill_inputs(&series, &mut series_idx, t, &mut inp);");
    m.push_back("        }");
    m.push_back("        nflow_step(&mut state, &inp, &mut out, t);");
    m.push_back("        write_row(&mut *w, t, &out);");
    if (s.hasStopBlock) {
        // stopSimulation: the stopping step's row is written, then the run ends.
        m.push_back("        if state.nflow_stop_requested != 0.0 { break; }");
    }
    m.push_back("    }");
    m.push_back("}");
    m.push_back("");
    return m;
}
//=============================================================================
} // namespace nflow
//=============================================================================
