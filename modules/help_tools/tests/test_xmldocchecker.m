%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
skip_testsuite(~have_xmldoc, 'XML documentation is missing')
%=============================================================================
[a, b, c] = xmldocchecker([modulepath('help_tools'), '/help/en_US/xml/chapter.xml']);
assert_istrue(a);
assert_istrue(isempty(b));
assert_istrue(isempty(c));
%=============================================================================
modules_path = [nelsonroot(), '/modules/'];
module_list = dir(modules_path);
for i = 1:length(module_list)
    if module_list(i).isdir && ~ismember(module_list(i).name, {'.', '..'})
        module_name = module_list(i).name;
        for lang = getavailablelanguages()'
            help_path = [modules_path, module_name, '/help/', lang{1}, '/xml/'];
            if isdir(help_path)
                xmlfiles = dir([help_path, '*.xml']);
                for j = 1:length(xmlfiles)
                    xmlfile = [help_path, xmlfiles(j).name];
                    [status, msg] = xmldocchecker(xmlfile);
                    assert_istrue(status);
                end
            end
        end
    end
end
%=============================================================================  
