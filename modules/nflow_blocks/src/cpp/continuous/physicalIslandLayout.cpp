//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// physicalIsland layout assembly: parses the params JSON into the IslandLayout
// every consumer shares. See physicalIsland.cpp for the DAE formulation and
// physicalIslandCodegen.cpp for the embedded-Newton emitters.
//=============================================================================
#include "physicalIslandShared.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace physisland {
        //=============================================================================
        static Kind
        parseKind(const std::string& s, bool& ok)
        {
            ok = true;
            if (s == "resistor") {
                return Kind::Resistor;
            }
            if (s == "conductor") {
                return Kind::Conductor;
            }
            if (s == "variableResistor") {
                return Kind::VariableResistor;
            }
            if (s == "variableConductor") {
                return Kind::VariableConductor;
            }
            if (s == "variableCapacitor") {
                return Kind::VariableCapacitor;
            }
            if (s == "variableInductor") {
                return Kind::VariableInductor;
            }
            if (s == "capacitor") {
                return Kind::Capacitor;
            }
            if (s == "inductor") {
                return Kind::Inductor;
            }
            if (s == "vsource") {
                return Kind::VSource;
            }
            if (s == "isource") {
                return Kind::ISource;
            }
            if (s == "vcvs") {
                return Kind::Vcvs;
            }
            if (s == "vccs") {
                return Kind::Vccs;
            }
            if (s == "ccvs") {
                return Kind::Ccvs;
            }
            if (s == "cccs") {
                return Kind::Cccs;
            }
            if (s == "opAmp") {
                return Kind::OpAmp;
            }
            if (s == "short") {
                return Kind::Short;
            }
            if (s == "idle") {
                return Kind::Idle;
            }
            if (s == "transformer") {
                return Kind::Transformer;
            }
            if (s == "gyrator") {
                return Kind::Gyrator;
            }
            if (s == "switch") {
                return Kind::Switch;
            }
            if (s == "diode") {
                return Kind::Diode;
            }
            if (s == "zdiode") {
                return Kind::ZDiode;
            }
            if (s == "idealDiode") {
                return Kind::IdealDiode;
            }
            if (s == "npn") {
                return Kind::Npn;
            }
            if (s == "pnp") {
                return Kind::Pnp;
            }
            if (s == "nmos") {
                return Kind::Nmos;
            }
            if (s == "pmos") {
                return Kind::Pmos;
            }
            if (s == "radiation") {
                return Kind::Radiation;
            }
            if (s == "currentSensor") {
                return Kind::CurrentSensor;
            }
            if (s == "voltageSensor") {
                return Kind::VoltageSensor;
            }
            if (s == "potentialSensor") {
                return Kind::PotentialSensor;
            }
            ok = false;
            return Kind::Resistor;
        }
        //=============================================================================
        static int
        jint(const json& o, const char* key, int dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<int>();
            }
            return dflt;
        }
        //=============================================================================
        static double
        jdbl(const json& o, const char* key, double dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<double>();
            }
            return dflt;
        }
        //=============================================================================
        IslandLayout
        buildLayout(const json& params)
        {
            IslandLayout L;
            L.numNodes = jint(params, "nodes", 0);
            if (L.numNodes < 1) {
                L.numNodes = 1;
            }
            // node potentials: node 0 = ground (no state), nodes 1..N-1 algebraic.
            L.eIndex.assign(static_cast<size_t>(L.numNodes), -1);
            int idx = 0;
            for (int n = 1; n < L.numNodes; ++n) {
                L.eIndex[static_cast<size_t>(n)] = idx++;
                L.id.push_back(0.0); // algebraic (KCL)
                L.ic.push_back(0.0);
            }
            if (params.contains("components") && params["components"].is_array()) {
                for (const auto& c : params["components"]) {
                    if (!c.contains("kind") || !c["kind"].is_string()) {
                        continue;
                    }
                    bool ok = false;
                    IslandComp comp;
                    comp.kind = parseKind(c["kind"].get<std::string>(), ok);
                    if (!ok) {
                        continue;
                    }
                    comp.a = jint(c, "a", 0);
                    comp.b = jint(c, "b", 0);
                    comp.c = jint(c, "c", 0);
                    comp.d = jint(c, "d", 0);
                    comp.port = jint(c, "port", -1);
                    comp.outPort = jint(c, "outPort", -1);
                    comp.ic = jdbl(c, "ic", 0.0);
                    switch (comp.kind) {
                    case Kind::Resistor:
                        comp.value = jdbl(c, "R", 1.0);
                        break;
                    case Kind::Conductor:
                        comp.value = jdbl(c, "G", 1.0);
                        break;
                    case Kind::VariableResistor:
                    case Kind::VariableConductor:
                        // Value comes from comp.port each step; no own state.
                        comp.value = 1.0;
                        break;
                    case Kind::VariableCapacitor:
                        // Charge formulation: state Q (differential) + i_C
                        // (algebraic); C comes from comp.port. ic = initial charge.
                        comp.stateIndex = idx;
                        L.id.push_back(1.0); // Q differential
                        L.ic.push_back(comp.ic);
                        L.id.push_back(0.0); // i_C algebraic
                        L.ic.push_back(0.0);
                        idx += 2;
                        break;
                    case Kind::VariableInductor:
                        // Flux formulation: state phi (differential) + i_L
                        // (algebraic); L comes from comp.port. ic = initial flux.
                        comp.stateIndex = idx;
                        L.id.push_back(1.0); // phi differential
                        L.ic.push_back(comp.ic);
                        L.id.push_back(0.0); // i_L algebraic
                        L.ic.push_back(0.0);
                        idx += 2;
                        break;
                    case Kind::Vccs:
                        comp.value = jdbl(c, "gain", 1.0);
                        break;
                    case Kind::Diode:
                        comp.value = jdbl(c, "Is", 1e-9); // saturation current
                        comp.vt = jdbl(c, "Vt", 0.025);
                        break;
                    case Kind::ZDiode:
                        comp.value = jdbl(c, "Is", 1e-9); // saturation current
                        comp.vt = jdbl(c, "Vt", 0.04);
                        comp.value2 = jdbl(c, "Vz", 5.0); // reverse breakdown voltage
                        break;
                    case Kind::IdealDiode:
                        // Hybrid ideal diode: one algebraic state s parametrizes
                        // the branch (off for s < 0, on otherwise); the mode flip
                        // at s = 0 is a zero-crossing surface.
                        comp.value = jdbl(c, "Ron", 1e-3); // on-resistance
                        comp.value2 = jdbl(c, "Goff", 1e-6); // off-conductance
                        comp.value3 = jdbl(c, "Vknee", 0.0); // knee voltage
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // s algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        L.hasIdealDiode = true;
                        break;
                    case Kind::Npn:
                    case Kind::Pnp:
                        // Ebers-Moll transistor: stateless nonlinear current stamp
                        // on the three terminals (a=C, b=E, c=B). No own state.
                        comp.value = jdbl(c, "Is", 1e-16); // saturation current
                        comp.vt = jdbl(c, "Vt", 0.025);
                        comp.value2 = jdbl(c, "Bf", 100.0); // forward beta
                        comp.value3 = jdbl(c, "Br", 1.0); // reverse beta
                        break;
                    case Kind::Nmos:
                    case Kind::Pmos:
                        // Square-law MOSFET: stateless nonlinear drain current on
                        // (a=D, b=S, c=G); the gate draws no current. No own state.
                        comp.value = jdbl(c, "Beta", 1e-3); // transconductance gain
                        comp.value2 = jdbl(c, "Vt", 1.0); // threshold voltage
                        break;
                    case Kind::Radiation:
                        comp.value = jdbl(c, "Gr", 1.0); // radiation conductance
                        break;
                    case Kind::Capacitor:
                        comp.value = jdbl(c, "C", 1.0);
                        comp.stateIndex = idx;
                        L.id.push_back(1.0); // v_C differential
                        L.ic.push_back(comp.ic);
                        L.id.push_back(0.0); // i_C algebraic
                        L.ic.push_back(0.0);
                        idx += 2;
                        break;
                    case Kind::Inductor:
                        comp.value = jdbl(c, "L", 1.0);
                        comp.stateIndex = idx;
                        L.id.push_back(1.0); // i_L differential
                        L.ic.push_back(comp.ic);
                        idx += 1;
                        break;
                    case Kind::VSource:
                        comp.value = jdbl(c, "V", 0.0);
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // branch current algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::Vcvs:
                        comp.value = jdbl(c, "gain", 1.0);
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // branch current algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::Ccvs:
                        // Current-controlled voltage source: output branch current
                        // i_out (si) + sense branch current i_cd (si+1), both
                        // algebraic. v_ab = gain i_cd, sense branch is a short.
                        comp.value = jdbl(c, "gain", 1.0);
                        comp.stateIndex = idx;
                        L.id.push_back(0.0);
                        L.ic.push_back(0.0);
                        L.id.push_back(0.0);
                        L.ic.push_back(0.0);
                        idx += 2;
                        break;
                    case Kind::Cccs:
                        // Current-controlled current source: only the sense branch
                        // current i_cd (si) is a state; the output is a current
                        // source gain i_cd injected a -> b.
                        comp.value = jdbl(c, "gain", 1.0);
                        comp.stateIndex = idx;
                        L.id.push_back(0.0);
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::OpAmp:
                        // Ideal op-amp (nullor): the output current is the free
                        // algebraic unknown; the residual forces e_+ = e_-.
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // output current algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::Short:
                    case Kind::Idle:
                        comp.stateIndex = idx; // branch current algebraic
                        L.id.push_back(0.0);
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::Transformer:
                        comp.value = jdbl(c, "n", 1.0); // turns ratio v1/v2
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // primary current i1 algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::Gyrator:
                        comp.value = jdbl(c, "G1", 1.0);
                        comp.value2 = jdbl(c, "G2", 1.0);
                        break; // no own state (currents are explicit in v)
                    case Kind::Switch:
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // branch current algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::CurrentSensor:
                        comp.value = 0.0; // ideal ammeter: 0 V across
                        comp.stateIndex = idx;
                        L.id.push_back(0.0); // branch current algebraic
                        L.ic.push_back(0.0);
                        idx += 1;
                        break;
                    case Kind::ISource:
                        comp.value = jdbl(c, "I", 0.0);
                        break;
                    case Kind::VoltageSensor:
                    case Kind::PotentialSensor:
                        break;
                    }
                    L.comps.push_back(comp);
                }
            }
            L.numStates = idx;
            L.diodeMode.assign(L.comps.size(), 0); // ideal diodes start off

            // Warm-start the node potentials so the IDACalcIC Newton starts near
            // the solution (a T^4 radiation term is flat at 0 and hard from a cold
            // start). A node tied to ground by a capacitor takes its initial
            // voltage, by a constant voltage source the source value. These are
            // only guesses; IDACalcIC still refines them.
            for (const IslandComp& comp : L.comps) {
                if (comp.kind == Kind::Capacitor && comp.b == 0 && comp.a > 0
                    && comp.a < L.numNodes) {
                    const int si = L.eIndex[static_cast<size_t>(comp.a)];
                    if (si >= 0 && si < (int)L.ic.size()) {
                        L.ic[static_cast<size_t>(si)] = comp.ic;
                    }
                } else if (comp.kind == Kind::VSource && comp.port < 0 && comp.b == 0 && comp.a > 0
                    && comp.a < L.numNodes) {
                    const int si = L.eIndex[static_cast<size_t>(comp.a)];
                    if (si >= 0 && si < (int)L.ic.size()) {
                        L.ic[static_cast<size_t>(si)] = comp.value;
                    }
                }
            }
            return L;
        }
        //=============================================================================
    } // namespace physisland
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
