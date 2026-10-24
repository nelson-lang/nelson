%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.expandModelicaDoc(documentJson)
%
% Public, bare-identifier wrapper around the private NFlow.internal.expandModelica so the
% editor's simulation path can expand `modelica` blocks (compile .mo -> FMU with
% OpenModelica) by name (feval) from the interpreter thread, exactly as sim()
% does before it hands the model to the engine. The GUI Run button drives the
% C++ engine directly and would otherwise reach the engine with a raw `modelica`
% block (unsupported_block). See NFlow.internal.expandModelica, sim.
%=============================================================================
function documentJson = expandModelicaDoc(documentJson)
  narginchk(1, 1);
  documentJson = NFlow.internal.expandModelica(documentJson);
end
%=============================================================================
