%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_excel() %#ok<NLS0026>
  persistent excel_detected %#ok<NLS0005>
  if isempty(excel_detected)
    excel_detected = false;
    if ismodule('com_engine')
      if ispc()
        try
          excelApplication = actxserver('Excel.Application');
          excelApplication.Quit;
          delete(excelApplication);
          excel_detected = true;
        catch
          excel_detected = false;
        end
      end
    end
  end
  r = excel_detected;
end
