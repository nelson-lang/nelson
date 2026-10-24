//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "NFlowFmuExport.hpp"
#include <string>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
TEST_CASE("buildFmuModelInfo assigns deterministic value references")
{
    std::vector<FmuInterfaceVar> inputs = { { "u", "Float64" } };
    std::vector<FmuInterfaceVar> outputs = { { "y", "Float64" } };
    FmuModelInfo info = buildFmuModelInfo("M", "tok", 0.01, 10.0, inputs, outputs);

    REQUIRE(info.variables.size() == 3);
    CHECK(info.variables[0].name == "time");
    CHECK(info.variables[0].causality == "independent");
    CHECK(info.variables[0].valueReference == 0u);
    CHECK(info.variables[1].name == "u");
    CHECK(info.variables[1].causality == "input");
    CHECK(info.variables[1].valueReference == 1u);
    CHECK(info.variables[1].start == "0");
    // outputs follow; calculated -> no start.
    CHECK(info.variables[2].name == "y");
    CHECK(info.variables[2].causality == "output");
    CHECK(info.variables[2].valueReference == 2u);
    CHECK(info.variables[2].start.empty());
}
//=============================================================================
TEST_CASE("buildModelDescriptionXml emits a well-formed FMI 3.0 description")
{
    std::vector<FmuInterfaceVar> inputs = { { "u", "Float64" } };
    std::vector<FmuInterfaceVar> outputs = { { "y", "Float64" } };
    FmuModelInfo info = buildFmuModelInfo("M", "tok", 0.01, 10.0, inputs, outputs);

    // The generation tool carries the product version; the golden must not
    // hardcode it.
    CHECK(info.generationTool.rfind("Nelson/nflow ", 0) == 0);

    const std::string expected
        = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<fmiModelDescription fmiVersion=\"3.0\" modelName=\"M\" instantiationToken=\"tok\" "
          "generationTool=\""
        + info.generationTool
        + "\">\n"
          "  <CoSimulation modelIdentifier=\"M\" canHandleVariableCommunicationStepSize=\"true\" "
          "canGetAndSetFMUState=\"true\" fixedInternalStepSize=\"0.01\"/>\n"
          "  <DefaultExperiment startTime=\"0\" stopTime=\"10\" stepSize=\"0.01\"/>\n"
          "  <ModelVariables>\n"
          "    <Float64 name=\"time\" valueReference=\"0\" causality=\"independent\" "
          "variability=\"continuous\" start=\"0\"/>\n"
          "    <Float64 name=\"u\" valueReference=\"1\" causality=\"input\" "
          "variability=\"continuous\" start=\"0\"/>\n"
          "    <Float64 name=\"y\" valueReference=\"2\" causality=\"output\" "
          "variability=\"continuous\"/>\n"
          "  </ModelVariables>\n"
          "  <ModelStructure>\n"
          "    <Output valueReference=\"2\"/>\n"
          "    <InitialUnknown valueReference=\"2\"/>\n"
          "  </ModelStructure>\n"
          "</fmiModelDescription>\n";

    CHECK(buildModelDescriptionXml(info) == expected);
}
//=============================================================================
TEST_CASE("integer variables use discrete variability and their FMI type element")
{
    std::vector<FmuInterfaceVar> inputs = { { "n", "Int64" } };
    std::vector<FmuInterfaceVar> outputs = { { "m", "UInt64" } };
    FmuModelInfo info = buildFmuModelInfo("K", "t2", 0.02, 5.0, inputs, outputs);
    const std::string xml = buildModelDescriptionXml(info);
    CHECK(xml.find("<Int64 name=\"n\"") != std::string::npos);
    CHECK(xml.find("<UInt64 name=\"m\"") != std::string::npos);
    CHECK(xml.find("variability=\"discrete\"") != std::string::npos);
}
//=============================================================================
TEST_CASE("XML special characters in the model name are escaped")
{
    FmuModelInfo info = buildFmuModelInfo("a&b", "tok", 0.01, 1.0, {}, {});
    const std::string xml = buildModelDescriptionXml(info);
    CHECK(xml.find("modelName=\"a&amp;b\"") != std::string::npos);
    CHECK(xml.find("a&b\"") == std::string::npos);
}
//=============================================================================
TEST_CASE("fmi3Functions.c wrapper maps value references to model fields")
{
    std::vector<FmuInterfaceVar> inputs = { { "u", "Float64" } };
    std::vector<FmuInterfaceVar> outputs = { { "y", "Float64" } };
    FmuModelInfo info = buildFmuModelInfo("M", "tok", 0.01, 10.0, inputs, outputs);
    const std::string c = buildFmi3FunctionsC(info);

    CHECK(c.find("#include \"model.c\"") != std::string::npos);
    CHECK(c.find("nflow_init(&c->state)") != std::string::npos);
    CHECK(c.find("nflow_step(&c->state, &c->input, &c->output, c->time)") != std::string::npos);
    CHECK(c.find("nflow_terminate(&((nflow_fmu_instance*)instance)->state)") != std::string::npos);
    CHECK(c.find("fmi3Instance fmi3InstantiateCoSimulation(") != std::string::npos);
    CHECK(c.find("fmi3Status fmi3DoStep(") != std::string::npos);
    // time is VR 0 in getFloat64; the input maps to c->input, the output to c->output.
    CHECK(c.find("case 0: values[i] = (fmi3Float64)c->time; break;") != std::string::npos);
    CHECK(c.find("case 1: c->input.u = values[i]; break;") != std::string::npos);
    CHECK(c.find("case 2: values[i] = c->output.y; break;") != std::string::npos);
    CHECK(c.find("fmi3GetInt64(") != std::string::npos);
    CHECK(c.find("fmi3SetInt64(") != std::string::npos);
    CHECK(c.find("fmi3GetUInt64(") != std::string::npos);
    // Importers resolve every symbol, so the full FMI 3.0 set is exported, including
    // fmi3GetOutputDerivatives (required for CS) and the typed get/set stubs.
    CHECK(c.find("fmi3GetOutputDerivatives(") != std::string::npos);
    CHECK(c.find("fmi3GetInt64(") != std::string::npos);
    CHECK(c.find("fmi3SetBoolean(") != std::string::npos);
    CHECK(c.find("fmi3InstantiateModelExchange(") != std::string::npos);
    CHECK(c.find("fmi3GetFMUState(") != std::string::npos);
}
//=============================================================================
TEST_CASE("buildDescription.xml lists the source files")
{
    const std::string expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                                 "<fmiBuildDescription fmiVersion=\"3.0\">\n"
                                 "  <BuildConfiguration modelIdentifier=\"M\">\n"
                                 "    <SourceFileSet language=\"C99\">\n"
                                 "      <SourceFile name=\"model.c\"/>\n"
                                 "      <SourceFile name=\"fmi3Functions.c\"/>\n"
                                 "    </SourceFileSet>\n"
                                 "  </BuildConfiguration>\n"
                                 "</fmiBuildDescription>\n";
    CHECK(buildBuildDescriptionXml("M") == expected);
}
//=============================================================================
