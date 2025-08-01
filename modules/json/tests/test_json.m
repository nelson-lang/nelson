%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = '"';
assert_isequal(jsondecode(jsonencode(r)), r)
%=============================================================================
r = '/';
assert_isequal(jsondecode(jsonencode(r)), r)
%=============================================================================
r = '\';
assert_isequal(jsondecode(jsonencode(r)), r)
%=============================================================================
r = '\r';
assert_isequal(jsondecode(jsonencode(r)), r)
%=============================================================================
r = jsonencode(char(13));
ref = '"\r"';
r2 = jsondecode(ref);
assert_isequal(r, ref);
assert_isequal(r2, char(13));
%=============================================================================
r = [14:31];
assert_isequal(double(jsondecode(jsonencode(char(r)))), r);
%=============================================================================
ref = '{"given":["Jim"]}';
r = jsonencode(jsondecode(ref));
assert_isequal(r, ref);
%=============================================================================
ref = '{"use":"usual","given":["Jim"]}';
r = jsonencode(jsondecode(ref));
assert_isequal(r, ref);
%=============================================================================
r ='{"address": [    {     "line": [        "534 Erewhon St"      ]    }  ]}';
R = jsondecode(r);
assert_isequal(class(R.address.line), 'cell');
assert_isequal(R.address.line{1}, '534 Erewhon St');
%=============================================================================
json_txt = fileread([modulepath('json'), '/examples/patient.json']);
st = jsondecode(json_txt);
r =  jsonencode(st);
ref = '{"resourceType":"Patient","id":"example","text":{"status":"generated","div":"<div xmlns=\"http://www.w3.org/1999/xhtml\">\n\t\t\t<table>\n\t\t\t\t<tbody>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Name</td>\n\t\t\t\t\t\t<td>Peter James \n              <b>Chalmers</b> (&quot;Jim&quot;)\n            </td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Address</td>\n\t\t\t\t\t\t<td>534 Erewhon, Pleasantville, Vic, 3999</td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Contacts</td>\n\t\t\t\t\t\t<td>Home: unknown. Work: (03) 5555 6473</td>\n\t\t\t\t\t</tr>\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<td>Id</td>\n\t\t\t\t\t\t<td>MRN: 12345 (Acme Healthcare)</td>\n\t\t\t\t\t</tr>\n\t\t\t\t</tbody>\n\t\t\t</table>\n\t\t</div>"},"identifier":{"use":"usual","type":{"coding":{"system":"http://hl7.org/fhir/v2/0203","code":"MR"}},"system":"urn:oid:1.2.36.146.595.217.0.1","value":"12345","period":{"start":"2001-05-06"},"assigner":{"display":"Acme Healthcare"}},"active":true,"name":[{"use":"official","family":"Chalmers","given":["Peter","James"]},{"use":"usual","given":["Jim"]},{"use":"maiden","family":"Windsor","given":["Peter","James"],"period":{"end":"2002"}}],"telecom":[{"use":"home"},{"system":"phone","value":"(03) 5555 6473","use":"work","rank":1},{"system":"phone","value":"(03) 3410 5613","use":"mobile","rank":2},{"system":"phone","value":"(03) 5555 8834","use":"old","period":{"end":"2014"}}],"gender":"male","birthDate":"1974-12-25","x_birthDate":{"extension":{"url":"http://hl7.org/fhir/StructureDefinition/patient-birthTime","valueDateTime":"1974-12-25T14:35:45-05:00"}},"deceasedBoolean":false,"address":{"use":"home","type":"both","text":"534 Erewhon St PeasantVille, Rainbow, Vic  3999","line":["534 Erewhon St"],"city":"PleasantVille","district":"Rainbow","state":"Vic","postalCode":"3999","period":{"start":"1974-12-25"}},"contact":{"relationship":{"coding":{"system":"http://hl7.org/fhir/v2/0131","code":"N"}},"name":{"family":"du Marché","x_family":{"extension":{"url":"http://hl7.org/fhir/StructureDefinition/humanname-own-prefix","valueString":"VV"}},"given":["Bénédicte"]},"telecom":{"system":"phone","value":"+33 (237) 998327"},"address":{"use":"home","type":"both","line":["534 Erewhon St"],"city":"PleasantVille","district":"Rainbow","state":"Vic","postalCode":"3999","period":{"start":"1974-12-25"}},"gender":"female","period":{"start":"2012"}},"managingOrganization":{"reference":"Organization/1"}}';
assert_isequal(r, ref);
%=============================================================================
