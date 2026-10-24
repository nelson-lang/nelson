//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Bridge that lets the nflow_blocks "fmu" block drive a real FMI 3.0
// Co-Simulation FMU, without nflow_blocks depending on fmi4c. Implements the
// IFmuComponent seam over FmiPackage / FmiInstance and registers the factory
// at load time. A .fmu archive is extracted with the ZIP-slip-hardened UnZip
// into a temporary directory owned (and cleaned) by the component.
//=============================================================================
#include "FmuComponent.hpp"
#include "FmiPackage.hpp"
#include "FmiInstance.hpp"
#include "Unzip.hpp"
#include "characters_encoding.hpp"
#include <atomic>
#include <chrono>
#include <filesystem>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        //=============================================================================
        std::filesystem::path
        makeOwnedStagingDir(
            const char* prefix, const std::filesystem::path& source, std::error_code& ec)
        {
            static std::atomic<unsigned long long> counter { 0 };
            const auto base = std::filesystem::temp_directory_path(ec);
            const auto tick = std::chrono::steady_clock::now().time_since_epoch().count();
            const std::string stem = source.stem().u8string();
            for (int attempt = 0; attempt < 64; ++attempt) {
                std::filesystem::path staging = base
                    / (std::string(prefix) + "_" + std::to_string(tick) + "_"
                        + std::to_string(counter.fetch_add(1)) + "_" + stem);
                ec.clear();
                if (std::filesystem::create_directory(staging, ec)) {
                    return staging;
                }
            }
            return {};
        }
        //=============================================================================
        class FmuComponentImpl : public IFmuComponent
        {
            struct Port
            {
                uint32_t vr;
                int dataType; // FmiDataType as int
                int width; // array element count (1 for scalar)
            };

        public:
            FmuComponentImpl() = default;

            ~FmuComponentImpl() override
            {
                m_instance.reset();
                m_package.reset();
                if (!m_stagingDir.empty()) {
                    std::error_code ec;
                    std::filesystem::remove_all(m_stagingDir, ec);
                }
            }

            bool
            load(const std::string& fmuPath, SimDiagnostic& diag)
            {
                std::error_code ec;
                std::filesystem::path p(std::filesystem::u8path(fmuPath));
                std::string unzippedDir;

                if (std::filesystem::is_directory(p, ec)) {
                    unzippedDir = fmuPath;
                } else if (std::filesystem::is_regular_file(p, ec)) {
                    std::filesystem::path staging = makeOwnedStagingDir("nflow_fmu", p, ec);
                    if (staging.empty()) {
                        diag.code = "fmu_extract";
                        diag.message = "Cannot create a temporary directory to extract the FMU.";
                        diag.fatal = true;
                        return false;
                    }
                    try {
                        wstringVector entries;
                        UnZip(utf8_to_wstring(fmuPath), staging.wstring(), entries);
                    } catch (...) {
                        std::filesystem::remove_all(staging, ec);
                        diag.code = "fmu_extract";
                        diag.message = "Failed to extract the FMU archive '" + fmuPath + "'.";
                        diag.fatal = true;
                        return false;
                    }
                    m_stagingDir = staging.string();
                    unzippedDir = m_stagingDir;
                } else {
                    diag.code = "fmu_not_found";
                    diag.message = "FMU path is neither a directory nor a file: '" + fmuPath + "'.";
                    diag.fatal = true;
                    return false;
                }

                std::string err;
                m_package = Fmi::FmiPackage::loadUnzipped("nflow_fmu", unzippedDir, err);
                if (!m_package) {
                    diag.code = "fmu_load";
                    diag.message
                        = err.empty() ? ("Failed to load FMU from '" + fmuPath + "'.") : err;
                    diag.fatal = true;
                    return false;
                }
                const Fmi::FmiModelInfo& mi = m_package->info();
                if (!mi.supportsCoSimulation) {
                    diag.code = "fmu_no_cosim";
                    diag.message = "FMU does not support Co-Simulation.";
                    diag.fatal = true;
                    return false;
                }
                for (const Fmi::FmiVariable& v : mi.variables) {
                    if (!isSupportedDataType(v.dataType)) {
                        // String/Binary/Clock variables cannot map onto a double-backed
                        // signal port; they are skipped (Clock-FMUs are rejected at
                        // package load).
                        continue;
                    }
                    const int width = std::max(1, v.arraySize);
                    if (v.causality == Fmi::FmiCausality::Output) {
                        m_outputVr.push_back(
                            { v.valueReference, static_cast<int>(v.dataType), width });
                    } else if (v.causality == Fmi::FmiCausality::Input) {
                        m_inputVr.push_back(
                            { v.valueReference, static_cast<int>(v.dataType), width });
                    } else if (v.causality == Fmi::FmiCausality::Parameter) {
                        m_paramVrByName[v.name]
                            = { v.valueReference, static_cast<int>(v.dataType) };
                    } else if (v.causality == Fmi::FmiCausality::StructuralParameter) {
                        m_structVrByName[v.name]
                            = { v.valueReference, static_cast<int>(v.dataType) };
                    }
                }
                return true;
            }

            // Split the block's parameter overrides into structural parameters (set
            // in Configuration Mode; a dimension bound to one resizes its array
            // ports) and regular tunable parameters. Returns false on an FMI error.
            bool
            applyStructuralParameters(const std::vector<std::string>& parameterNames,
                const std::vector<double>& parameterValues, SimDiagnostic& diag)
            {
                std::vector<std::pair<uint32_t, uint64_t>> overridesByVr;
                std::vector<size_t> structIdx;
                for (size_t i = 0; i < parameterNames.size(); ++i) {
                    auto it = m_structVrByName.find(parameterNames[i]);
                    if (it != m_structVrByName.end()) {
                        structIdx.push_back(i);
                        overridesByVr.emplace_back(
                            it->second.first, (uint64_t)std::llround(parameterValues[i]));
                    }
                }
                if (!structIdx.empty()) {
                    std::string err;
                    if (!m_instance->enterConfigurationMode(err)) {
                        diag.code = "fmu_config";
                        diag.message = err;
                        diag.fatal = true;
                        return false;
                    }
                    for (size_t i : structIdx) {
                        auto it = m_structVrByName.find(parameterNames[i]);
                        if (!m_instance->setTyped(it->second.second, { it->second.first },
                                { parameterValues[i] }, err)) {
                            diag.code = "fmu_set_structural";
                            diag.message = err;
                            diag.fatal = true;
                            return false;
                        }
                    }
                    if (!m_instance->exitConfigurationMode(err)) {
                        diag.code = "fmu_config";
                        diag.message = err;
                        diag.fatal = true;
                        return false;
                    }
                    // Recompute the effective port widths: an array dimension bound
                    // to one of the overridden structural parameters changed size.
                    for (Port& p : m_inputVr) {
                        refreshPortWidth(p, overridesByVr);
                    }
                    for (Port& p : m_outputVr) {
                        refreshPortWidth(p, overridesByVr);
                    }
                }
                return true;
            }

            void
            refreshPortWidth(
                Port& p, const std::vector<std::pair<uint32_t, uint64_t>>& overridesByVr)
            {
                for (const Fmi::FmiVariable& v : m_package->info().variables) {
                    if (v.valueReference == p.vr) {
                        p.width = std::max(1, Fmi::effectiveArraySize(v, overridesByVr));
                        return;
                    }
                }
            }

            static bool
            isSupportedDataType(Fmi::FmiDataType t)
            {
                switch (t) {
                case Fmi::FmiDataType::Float64:
                case Fmi::FmiDataType::Float32:
                case Fmi::FmiDataType::Int8:
                case Fmi::FmiDataType::UInt8:
                case Fmi::FmiDataType::Int16:
                case Fmi::FmiDataType::UInt16:
                case Fmi::FmiDataType::Int32:
                case Fmi::FmiDataType::UInt32:
                case Fmi::FmiDataType::Int64:
                case Fmi::FmiDataType::UInt64:
                case Fmi::FmiDataType::Boolean:
                case Fmi::FmiDataType::Enumeration:
                    return true;
                default:
                    return false;
                }
            }

            bool
            initialize(double startTime, double stopTime,
                const std::vector<std::string>& parameterNames,
                const std::vector<double>& parameterValues, SimDiagnostic& diag) override
            {
                std::string err;
                m_instance = m_package->instantiateCoSimulation("nflow_fmu", false, err);
                if (!m_instance) {
                    diag.code = "fmu_instantiate";
                    diag.message = err.empty() ? "fmi3InstantiateCoSimulation failed." : err;
                    diag.fatal = true;
                    return false;
                }
                if (!applyStructuralParameters(parameterNames, parameterValues, diag)) {
                    return false;
                }
                const bool stopDefined = stopTime > startTime;
                const bool tolDefined = m_relTol > 0.0;
                if (!m_instance->enterInitializationMode(tolDefined, tolDefined ? m_relTol : 0.0,
                        startTime, stopDefined, stopTime, err)) {
                    diag.code = "fmu_init";
                    diag.message = err;
                    diag.fatal = true;
                    return false;
                }
                // Tunable parameters are applied in initialization mode, typed per variable.
                if (!parameterNames.empty()) {
                    for (size_t i = 0; i < parameterNames.size(); ++i) {
                        auto it = m_paramVrByName.find(parameterNames[i]);
                        if (it == m_paramVrByName.end()) {
                            continue;
                        }
                        if (!m_instance->setTyped(it->second.second, { it->second.first },
                                { parameterValues[i] }, err)) {
                            diag.code = "fmu_set_param";
                            diag.message = err;
                            diag.fatal = true;
                            return false;
                        }
                    }
                }
                if (!m_instance->exitInitializationMode(err)) {
                    diag.code = "fmu_init";
                    diag.message = err;
                    diag.fatal = true;
                    return false;
                }
                return true;
            }

            int
            numInputs() const override
            {
                return static_cast<int>(m_inputVr.size());
            }

            int
            numOutputs() const override
            {
                return static_cast<int>(m_outputVr.size());
            }

            int
            inputPortWidth(int p) const override
            {
                return (p >= 0 && p < (int)m_inputVr.size()) ? m_inputVr[p].width : 1;
            }

            int
            outputPortWidth(int p) const override
            {
                return (p >= 0 && p < (int)m_outputVr.size()) ? m_outputVr[p].width : 1;
            }

            bool
            configurePorts(const std::vector<std::string>& inputNames,
                const std::vector<std::string>& outputNames, SimDiagnostic& diag) override
            {
                const Fmi::FmiModelInfo& mi = m_package->info();
                auto findVar = [&](const std::string& name) -> const Fmi::FmiVariable* {
                    for (const Fmi::FmiVariable& v : mi.variables) {
                        if (v.name == name) {
                            return &v;
                        }
                    }
                    return nullptr;
                };
                auto buildPorts = [&](const std::vector<std::string>& names, const char* which,
                                      std::vector<Port>& out) -> bool {
                    std::vector<Port> ports;
                    for (const std::string& name : names) {
                        const Fmi::FmiVariable* v = findVar(name);
                        if (v == nullptr) {
                            diag.code = "fmu_port_name";
                            diag.message
                                = std::string("FMU has no ") + which + " variable '" + name + "'.";
                            diag.fatal = true;
                            return false;
                        }
                        if (!isSupportedDataType(v->dataType)) {
                            diag.code = "fmu_port_type";
                            diag.message = std::string("FMU ") + which + " variable '" + name
                                + "' has an unsupported type.";
                            diag.fatal = true;
                            return false;
                        }
                        ports.push_back({ v->valueReference, static_cast<int>(v->dataType),
                            std::max(1, v->arraySize) });
                    }
                    out = std::move(ports);
                    return true;
                };
                // An empty list keeps the full enumeration.
                if (!inputNames.empty() && !buildPorts(inputNames, "input", m_inputVr)) {
                    return false;
                }
                if (!outputNames.empty() && !buildPorts(outputNames, "output", m_outputVr)) {
                    return false;
                }
                return true;
            }

            bool
            setInputs(const double* values, int n, SimDiagnostic& diag) override
            {
                if (n <= 0 || m_inputVr.empty() || !m_instance) {
                    return true;
                }
                std::string err;
                int at = 0;
                for (const Port& port : m_inputVr) {
                    if (at >= n) {
                        break;
                    }
                    const int w = std::min(port.width, n - at);
                    std::vector<double> vals(values + at, values + at + w);
                    if (!m_instance->setTyped(port.dataType, { port.vr }, vals, err)) {
                        diag.code = "fmu_set";
                        diag.message = err;
                        return false;
                    }
                    at += port.width;
                }
                return true;
            }

            bool
            getOutputs(double* out, int n, SimDiagnostic& diag) override
            {
                if (n <= 0 || m_outputVr.empty() || !m_instance) {
                    return true;
                }
                std::string err;
                int at = 0;
                for (const Port& port : m_outputVr) {
                    if (at >= n) {
                        break;
                    }
                    std::vector<double> value;
                    if (!m_instance->getTyped(port.dataType, { port.vr }, value, err, port.width)) {
                        diag.code = "fmu_get";
                        diag.message = err;
                        return false;
                    }
                    const int w = std::min(port.width, n - at);
                    for (int k = 0; k < w; ++k) {
                        out[at + k] = (k < (int)value.size()) ? value[k] : 0.0;
                    }
                    at += port.width;
                }
                return true;
            }

            bool
            doStep(double currentTime, double stepSize, SimDiagnostic& diag) override
            {
                if (!m_instance) {
                    return false;
                }
                Fmi::FmiStepResult step;
                std::string err;
                if (!m_instance->doStep(currentTime, stepSize, step, err)) {
                    diag.code = "fmu_step";
                    diag.message = err;
                    return false;
                }
                if (step.earlyReturn) {
                    diag.code = "fmu_early_return";
                    diag.message
                        = "FMU returned early at t=" + std::to_string(step.lastSuccessfulTime)
                        + "; fixed-step NFlow blocks cannot reschedule the communication step.";
                    return false;
                }
                // The step succeeded but the FMU asked to terminate: stop the run early and keep
                // the results instead of stepping a terminated FMU.
                if (step.terminateRequested) {
                    diag.requestStop = true;
                    diag.code = "fmu_terminate";
                    diag.message = "FMU requested termination at t="
                        + std::to_string(currentTime + stepSize);
                }
                return true;
            }

            void
            setSolverTolerance(double relTol) override
            {
                m_relTol = relTol;
            }

            bool
            canSaveState() const override
            {
                return m_package && m_package->info().canGetAndSetState;
            }

            bool
            saveState(SimDiagnostic& diag) override
            {
                if (!m_instance) {
                    return false;
                }
                std::string err;
                if (!m_instance->saveState(err)) {
                    diag.code = "fmu_save_state";
                    diag.message = err;
                    return false;
                }
                return true;
            }

            bool
            restoreState(SimDiagnostic& diag) override
            {
                if (!m_instance) {
                    return false;
                }
                std::string err;
                if (!m_instance->restoreState(err)) {
                    diag.code = "fmu_restore_state";
                    diag.message = err;
                    return false;
                }
                return true;
            }

        private:
            double m_relTol = 0.0; // diagram relative tolerance (0 => FMU default)
            std::string m_stagingDir;
            std::unique_ptr<Fmi::FmiPackage> m_package;
            std::unique_ptr<Fmi::FmiInstance> m_instance;
            std::vector<Port> m_inputVr;
            std::vector<Port> m_outputVr;
            // (valueReference, FmiDataType as int) per parameter (parameters stay scalar).
            std::unordered_map<std::string, std::pair<uint32_t, int>> m_paramVrByName;
            // Structural parameters (settable only in Configuration Mode; array
            // dimensions may be bound to their values).
            std::unordered_map<std::string, std::pair<uint32_t, int>> m_structVrByName;
        };
        //=============================================================================
        struct FmuFactoryRegistrar
        {
            FmuFactoryRegistrar()
            {
                registerFmuComponentFactory(
                    [](const std::string& fmuPath,
                        SimDiagnostic& diag) -> std::unique_ptr<IFmuComponent> {
                        auto comp = std::make_unique<FmuComponentImpl>();
                        if (!comp->load(fmuPath, diag)) {
                            return nullptr;
                        }
                        return comp;
                    });
            }
        };
        //=============================================================================
        static FmuFactoryRegistrar g_fmuFactoryRegistrar;
        //=============================================================================
    } // namespace
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
