%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(varargin)
  narginchk(1, 1);
  nargoutchk(0, 0);
  td = varargin{1};
  if ~isa(td, 'tdigest')
    error(_("First argument must be a 'tdigest' object."));
  end
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  if strcmp(LineSpacing, 'loose')
    msgfmt = '\n  tdigest object with compression factor: %d\n'; 
  else
    msgfmt = '  tdigest object with compression factor: %d\n'; 
  end
  st = struct(td);
  fprintf(msgfmt, st.compression);
  fprintf("  Total weight: %d\n", st.totalWeight);
  fprintf("  Number of centroids: %d\n", length(st.means));
  if strcmp(LineSpacing, 'loose')
    fprintf("\n");
  end
end
%=============================================================================
