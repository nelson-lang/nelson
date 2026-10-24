%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [pstates, logpseq] = hmmdecode(seq, trans, emis)
  narginchk(3, 3);
  nargoutchk(0, 2);
  [trans, emis, ~, nsymbols] = __hmm_check_model(trans, emis);
  seq = __hmm_check_sequence(seq, nsymbols, 'SEQ');
  [gamma, ~, logpseq] = __hmm_forward_backward(seq, trans, emis);
  pstates = gamma';
end
%=============================================================================
