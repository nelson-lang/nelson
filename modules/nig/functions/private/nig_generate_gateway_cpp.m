%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_generate_gateway_cpp(NIG_FUNCTIONS)
  txt = nig_header_license();
  txt{end + 1} = ['// Generated by Nelson Interface Generator ', nig_version()];
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = '#include "NelsonGateway.hpp"';
  for k = NIG_FUNCTIONS(:)'
    txt{end + 1} = ['#include "', k.NELSON_NAME, 'Builtin.hpp"'];
  end
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'using namespace Nelson;';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = ['const std::wstring gatewayName = L"', NIG_FUNCTIONS(1).MODULE_NAME, '";'];
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'static const nlsGateway gateway[] =';
  txt{end + 1} = '{';
  for k = NIG_FUNCTIONS(:)'
    if isfield(k, 'NELSON_NAMESPACE')
      cppFunctionName = ['Nelson::', k.NELSON_NAMESPACE, 'Gateway::', k.NELSON_NAME, 'Builtin'];
    else
      cppFunctionName = ['Nelson::', k.NELSON_NAME, 'Builtin'];
    end
    nb_argin = int2str(nig_nargin(k));
    nb_argout = int2str(nig_nargout(k));
    txt{end + 1} = ['    { "', k.NELSON_NAME, '", (void*)', cppFunctionName, ', ', nb_argout, ', ', nb_argin, '},'];
  end
  txt{end + 1} = '};';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'NLSGATEWAYFUNC(gateway)';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'NLSGATEWAYINFO(gateway)';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'NLSGATEWAYREMOVE(gateway)';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'NLSGATEWAYNAME()';
  txt{end + 1} = '//=============================================================================';
end
%=============================================================================
