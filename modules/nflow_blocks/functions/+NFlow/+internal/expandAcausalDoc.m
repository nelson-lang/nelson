%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.expandAcausalDoc(documentJson)
%
% GUI Run bridge: lower every acausal (physical-pin) and planar component of a
% diagram into the engine's native island blocks, so the C++ Run path can hand
% the C++ engine a document it already understands. Runs the electrical /
% thermal / translational / rotational lowering (NFlow.internal.acausalExpand) then the
% planar lowering (NFlow.internal.planarExpand). Both are zero-cost when the diagram has
% no physical wires, so a purely causal model returns unchanged.
%=============================================================================
function documentJson = expandAcausalDoc(documentJson)
  documentJson = NFlow.internal.acausalExpand(documentJson);
  documentJson = NFlow.internal.planarExpand(documentJson);
end
%=============================================================================
