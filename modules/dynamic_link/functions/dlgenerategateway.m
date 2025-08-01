%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dlgenerategateway(destinationdir, module_name, builtin_table)
  if ~isdir(destinationdir)
    error(_('Invalid directory.'));
  end
  if ~ischar(module_name)
    error(_('Wrong type for argument #2: string expected.'));
  end
  [status, builtin_table] = checkAndPrepareBuiltinTable(builtin_table);
  if ~status
    error(_('Invalid builtin list.'));
  end
  txt = template(module_name, builtin_table);
  filewrite([destinationdir, '/Gateway.cpp'], txt);
end
%=============================================================================
function [status, builtin_table_modified] = checkAndPrepareBuiltinTable(builtin_table)
  builtin_table_modified = {};
  status = true;
  if iscell(builtin_table)
    for k = builtin_table(:)'
      if length(k{1}) == 3
        builtin_type = 'CPP_BUILTIN';
      else
        builtin_type = k{1}{4};
      end
      is_supported = strcmp(builtin_type, 'CPP_BUILTIN') == true || strcmp(builtin_type, 'CPP_BUILTIN_WITH_EVALUATOR') == true;
      if ~is_supported
        status = false;
        return;
      end
      element_to_add = { k{1}{1}, double(k{1}{2}), double(k{1}{3}), builtin_type };
      builtin_table_modified{end + 1} = element_to_add;
    end
  else
    status = false;
  end
end
%=============================================================================
function txt = template(module_name, builtin_table)
  txt = {'//============================================================================='};
  txt = [txt; '// Copyright (c) 2018 Allan CORNET (Nelson)'];
  txt = [txt; '//'];
  txt = [txt; '// Redistribution and use in source and binary forms, with or without'];
  txt = [txt; '// modification, are permitted provided that the following conditions are met:'];
  txt = [txt; '//'];
  txt = [txt; '// Redistributions of source code must retain the above copyright notice, this'];
  txt = [txt; '// list of conditions and the following disclaimer. Redistributions in binary'];
  txt = [txt; '// form must reproduce the above copyright notice, this list of conditions and'];
  txt = [txt; '// the following disclaimer in the documentation and/or other materials'];
  txt = [txt; '// provided with the distribution. Neither the name of the nor the names of'];
  txt = [txt; '// its contributors may be used to endorse or promote products derived from'];
  txt = [txt; '// this software without specific prior written permission.'];
  txt = [txt; '//'];
  txt = [txt; '// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"'];
  txt = [txt; '// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE'];
  txt = [txt; '// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE'];
  txt = [txt; '// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE'];
  txt = [txt; '// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR'];
  txt = [txt; '// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF'];
  txt = [txt; '// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS'];
  txt = [txt; '// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN'];
  txt = [txt; '// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)'];
  txt = [txt; '// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE'];
  txt = [txt; '// POSSIBILITY OF SUCH DAMAGE.'];
  txt = [txt; '//============================================================================='];
  txt = [txt; '// generated by dlgenerategateway'];
  txt = [txt; '//============================================================================='];
  txt = [txt; '#include "NelsonGateway.hpp"'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'using namespace Nelson;'];
  txt = [txt; '//============================================================================='];
  for k = builtin_table(:)'
    name = k{1}{1};
    builtintype = k{1}{4};
    if strcmp(builtintype, 'CPP_BUILTIN_WITH_EVALUATOR') == true
      txt = [txt; ['extern ArrayOfVector ', name, 'Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);']];
    else
      txt = [txt; ['extern ArrayOfVector ', name, 'Builtin(int nLhs, const ArrayOfVector& argIn);']];
    end
  end
  txt = [txt; '//============================================================================='];
  txt = [txt; 'const std::wstring gatewayName = L"', module_name, '";'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'static const nlsGateway gateway[] ='];
  txt = [txt; '{'];
  for k = builtin_table(:)'
    name = k{1}{1};
    nboutputs = k{1}{2};
    nbinputs = k{1}{3};
    builtintype = k{1}{4};
    line = ['    { "', name, '", (void*)', name, 'Builtin, ', int2str(nboutputs), ', ', int2str(nbinputs), ', ', builtintype ,' },'];
    txt = [txt; line];
  end
  txt = [txt; '};'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'NLSGATEWAYFUNC(gateway)'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'NLSGATEWAYINFO(gateway)'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'NLSGATEWAYREMOVE(gateway)'];
  txt = [txt; '//============================================================================='];
  txt = [txt; 'NLSGATEWAYNAME()'];
  txt = [txt; '//============================================================================='];
  
end
%=============================================================================
