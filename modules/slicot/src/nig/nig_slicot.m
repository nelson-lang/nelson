%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

% Nelson Interface Generator (NIG)


FILES = {
'nig_sb10jd.json';
'nig_mb02md.json';
'nig_sg02ad.json';
'nig_sb04qd.json';
'nig_sb04md.json';
'nig_mb03od.json';
'nig_mb03rd.json';
'nig_mb03pd.json';
'nig_ab01od.json';
'nig_ab04md.json';
'nig_ab08nd.json';
'nig_sb01bd.json';
'nig_sb03od.json';
'nig_sb03md.json';
'nig_sb02od.json';
'nig_mb04md.json';
'nig_ab07nd.json';
'nig_ab07nd.json';
'nig_ag08bd.json';
'nig_tg01ad.json';
'nig_mc01td.json';
'nig_mb04gd.json';
'nig_mb05od.json';
'nig_tb01id.json';
};

NIG_FUNCTIONS = struct([]);
for f = FILES(:)'
  NIG_FUNCTION = jsondecode(f{1}, '-file');
  if isempty(NIG_FUNCTIONS)
    NIG_FUNCTIONS = NIG_FUNCTION;
  else
    NIG_FUNCTIONS = [NIG_FUNCTIONS; NIG_FUNCTION];
  end
end

nig(NIG_FUNCTIONS, modulepath('slicot'))
