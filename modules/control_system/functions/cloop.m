%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function model = cloop(varargin)
  % model = cloop(sys)
  % model = cloop(sys, sign)
  % model = cloop(sys, outputs, inputs)
  narginchk(1, 3);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  
  if nargin == 1
    sgn = -1; 
  elseif nargin == 2
    sgn = sign(varargin{2});
  else
    inputs = varargin{2};
    outputs = varargin{3};
    sgn = sign(varargin{3});
  end
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  if isa(sys, 'ss')
    a = sys.A;
    b = sys.B;
    c = sys.C;
    d = sys.D;
    [ny, nu] = size(d);
    if (nargin == 1)
      outputs = 1:ny;
      inputs = [1:nu];
      sgn = -ones(1, length(inputs));
    end
    if (nargin == 2)  
      outputs = 1:ny;
      inputs = [1:nu];
      sgn = sgn * ones(1, length(inputs));
    end
    
    nin = length(inputs);
    nout = length(outputs);
    if nin ~= nout
      error(_('The number of feedback inputs and outputs are not equal.'));
    end
    [nx, na] = size(a);
    
    S = [a, b; c, d];
    Bu = S(:,[nx+inputs]); Cy = S([nx+outputs],:);
    if ~isempty(Cy)
      Cy(sgn == -1, :) = -Cy(sgn == -1, :);
      E = eye(nout) - Cy(:, [nx+inputs]);
      Cy = E\Cy;
      Sc = S + Bu*Cy;
      
      ac = Sc(1:nx, 1:nx);
      bc = Sc(1:nx, nx+1:nx+nu);
      cc = Sc(nx+1:nx+ny, 1:nx);
      dc = Sc(nx+1:nx+ny, nx+1:nx+nu);
      model = ss(ac, bc, cc, dc, sys.Ts);  
    else
      model = sys;
    end
  else % isa(sys, 'tf')
    numerator = sys.Numerator{1}; 
    denominator = sys.Denominator{1} - sgn * sys.Numerator{1};
    model = tf(numerator, denominator, sys.Ts);
    model.Variable = sys.Variable;  
  end
end
