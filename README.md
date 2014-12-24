calc.js
=======
calc.js is a framework that helps you implement calculations (by generating the appropriate JavaScript based on formulas that you provide) on fields done in an HTML form. It runs separately from your web application, but will require some REST APIs implemented in the system that uses calc.js. The core parts are built using mainly Erlang and Django templates. It requires minimal setup and should be simple to use.

We first started out calc.js as a seperate module for one of our client's projects, but that approach was very specific to the particular project. We decided to open source a much more generic approach to allow for others who might need a similar framework.

# Roadmap
## V1 (December 2014)
* Initial release
* Only works with mnesia for the DB

## V2 (Early 2015)
* Compatibility with other DBs (MySQL, riak, etc.)

## V3 (Mid 2015)
* JS executed on server-side
* Unit test cases

## V4 (Late 2015)
* Security enhancements

# Prerequisites
* Erlang R16B01 and above (currently tested only with R16B01)
* wget (not installed by default for Macs)

# Installation
1. Modify rel/sys.config to match your environment (calcjs:http_port, calcjs:admin_port, mnesia:dir).
1. Run the following commands
<pre><code>cd calcjs
make clean && make
./_rel/calcjs_release/bin/calcjs_release start
./_rel/calcjs_release/bin/calcjs_release attach
#[In the attached erlang console]
application:stop(mnesia).
calcjs_app:install([node() | nodes()]).
application:start(mnesia).
mnesia:system_info().
</code></pre>

# Getting started
We will go over a simple example of how to use calc.js.
All source files are located inside the "sample" folder.

## Implement an API that returns JSON describing the fields in a form
The format of the JSON should be something like
<pre><code>{"fieldInfo":
    [{"name":"field1","type":0,"repeating":false},
     {"name":"field2","type":0,"repeating":false},
     {"name":"field3","type":0,"repeating":false}]}
</code></pre>
<table border="1">
    <th>key</th>
    <th>description</th>
    <tr>
        <td>name</td>
        <td>The name of the field.</td>
    </tr>
    <tr>
        <td>type</td>
        <td>
            <p>The data type</p>
            <p>
                <table>
                    <th>value</th>
                    <th>description</th>
                    <tr>
                        <td>0</td>
                        <td>number</td>
                    </tr>
                    <tr>
                        <td>1</td>
                        <td>decimal</td>
                    </tr>
                    <tr>
                        <td>2</td>
                        <td>string</td>
                    </tr>
                    <tr>
                        <td>3</td>
                        <td>boolean</td>
                    </tr>
                    <tr>
                        <td>4</td>
                        <td>zipcode (split into zip1 and zip2 fields)</td>
                    </tr>
                    <tr>
                        <td>5</td>
                        <td>Japanes date (split into era, year, month, day fields)</td>
                    </tr>
                    <tr>
                        <td>6</td>
                        <td>Japanese date with only era, yy, mm fields</td>
                    </tr>
                    <tr>
                        <td>7</td>
                        <td>A value with an associated label with value and label fields (used for select boxes)</td>
                    </tr>
                    <tr>
                        <td>8</td>
                        <td>Japanese data with only era and yy fields</td>
                    </tr>
                    <tr>
                        <td>9</td>
                        <td>A value that references the ID of some XML data (to be removed)</td>
                    </tr>
                    <tr>
                        <td>10</td>
                        <td>A telephone number with tel1, tel2, tel3 fields</td>
                    </tr>
                    <tr>
                        <td>11</td>
                        <td>Japanese date with mm and dd fields</td>
                    </tr>
                    <tr>
                        <td>12</td>
                        <td>Japanese date with era, yy, mm, dd fields</td>
                    </tr>
                </table>
            </p>
        </td>
    </tr>
    <tr>
        <td>repeating</td>
        <td>Whether or not this field repeats (is an array with a common id/name ending in "_{the nth repetition starting from 1}")</td>
    </tr>
</table>
In sampleFieldInfoAPI.php, we implement a dummy service that just returns the above sample response.
Ideally, you should make your API RESTful and accept parameters to give data for different forms.
<pre><code>    $form_id = $_GET["form_id"];
    if ($form_id == "SampleForm1") {
    	$arr = array ('fieldInfo' => array (
    			array ('name' => 'field1', 'type' => 0, 'repeating' => false),
    			array ('name' => 'field2', 'type' => 0, 'repeating' => false),
    			array ('name' => 'field3', 'type' => 0, 'repeating' => false)
    		)
    	);
    } else if ($form_id == "SampleForm2") {
    	$arr = array ('fieldInfo' => array (
    			array ('name' => 'field1', 'type' => 0, 'repeating' => false),
    			array ('name' => 'field2', 'type' => 0, 'repeating' => false),
    			array ('name' => 'field3', 'type' => 0, 'repeating' => false)
    		)
    	);
    }
    echo json_encode($arr);
</code></pre>
## Implement an API that returns JSON describing data for the form
This will not be used when you are viewing the current form, but only when you are trying to get data to reference from other forms.
<pre><code>{"data":
        [{"name":"field1","value":111,"type":0},
         {"name":"field2","value":222,"type":0},
         {"name":"field3","value":333,"type":0}],
 "correctedFields":["field1"]}</code></pre>
<table border="1">
    <th>key</th>
    <th>description</th>
    <tr>
        <td>name</td>
        <td>The name of the field.</td>
    </tr>
    <tr>
        <td>value</td>
        <td>The value of the field</td>
    </tr>
    <tr>
        <td>type</td>
        <td>The type of the field</td>
    </tr>
</table>
In SampleDataRetrievalAPI.php, we provide a dummy service that just returns the above sample JSON.
For actual implementation, it would be better to generate a different response for the different forms you have, and you should probaby pass in some ID or other parameters to distinguish that data.
<pre><code>    $arr = array ( 'data' => array (
    	array ('name' => "field1", 'value' => 111, 'type' => 0),
    	array ('name' => "field2", 'value' => 222, 'type' => 0),
    	array ('name' => "field3", 'value' => 333, 'type' => 0)
    ), 'correctedFields' => array('field1'));
    echo json_encode($arr);
</code></pre>
## Implement an API that saves your data
This should generally be implemented somewhere in your service already, so we just provide some dummy service that has a TODO in SampleSaveDataAPI.php.
We just need to make sure that it works with posting JSON in the following sample format.
<pre><code>{"data":
    [{"field1":111,
     "field2":222,
     "field3":333}]}
</code></pre>
<pre><code>//TODO: implement some code here to save data
echo "";
</code></pre>
## Create your HTML form
* Include the calc.js css located by default at localhost:8003/assets/css/calcjs.css.
This css file in particular just provides css to distinguish fields that are calculated (green-colored inputs) and tan for calculated fields corrected by the user.

* Include the dependent library files in this order:
<ol>
<li>localhost:8003/assets/js/jquery.min.js</li>
<li>localhost:8003/assets/js/jquery.inputmask.bundle.js</li>
<li>localhost:8003/assets/js/BigDecimal-all-last.min.js</li>
</ol>

* Add the provided calculation utility library located at localhost:8003/assets/js/calcjs.utils.js

* Add the generated js (localhost:8003/js/SampleForm1?dependent_object_id=1 in the included sample)

* In the HTML form
<ol>
<li>Add a hidden input field with "id" and "name" set to "corrected_fields".</li>
<li>Set the class "commafied" for numerical inputs that you want to be commafied.</li>
<li>Add the "data-inputmask" attribute ("'alias': 'integer', 'integerDigits': 15, 'allowMinus':true, 'groupSeparator': ',', 'autoGroup': true, 'groupSize': 3" in the sample).</li>
</ol>
Please refer to sample/sampleForm.html for the full source.

## Configure calc.js through the admin UI
With the default settings, you can access the admin UI by accessing http://localhost:8004/admin.
Clicking the "API Configuration" link will lead you to a screen where you can set whether the generated JavaScript will work with the "id" or "name" attribute for input elements.

Clicking the "Forms List" link will direct you to a list of your registered forms.
You can create a new form by clicking on the "Create New" link at the bottom.

You can also import the sample form data by copying over the files inside sample/data in your mnesia directory (if you do that, you won't need to execute the installation commands inside the erlang console).

Please fill out the following fields as necessary for your form:
<table border="1">
    <th>name</th>
    <th>description</th>
    <tr>
        <td>ID</td>
        <td>The ID used to identify your form (must be unique)</td>
    </tr>
    <tr>
        <td>Description</td>
        <td>A description of your form (only used for internal reference)</td>
    </tr>
    <tr>
        <td>Active</td>
        <td>Whether or not this entire form's calculations should be enabled or now</td>
    </tr>
    <tr>
        <td>Data Retrieval API</td>
        <td>The API to be used that returns JSON data for the form</td>
    </tr>
    <tr>
        <td>post_api</td>
        <td>The API used to save the form's data (by POSTing JSON)</td>
    </tr>
    <tr>
        <td>field_info_api</td>
        <td>The API to use to retrieve the form field's information in JSON</td>
    </tr>
    <tr>
        <td>Dependent Forms</td>
        <td>
            <div>Information about forms that this form depends on in it's calculations (fields from other forms that need to be referenced)</div>
            <table>
                <th>name</th>
                <th>description</th>
                <tr>
                    <td>Form Name</td>
                    <td>The name of the form (should be the same as the "ID" field registered into calc.js)</td>
                </tr>
                <tr>
                    <td>Data Retrieval API</td>
                    <td>The API to be used that returns JSON data for the form</td>
                </tr>
                <tr>
                    <td>POST API</td>
                    <td>The API used to save the form's data (by POSTing JSON)</td>
                </tr>
                <tr>
                    <td>Field Info API</td>
                    <td>The API to use to retrieve the form field's information in JSON</td>
                </tr>
            </table>
        </td>
    </tr>
    <tr>
        <td>Calculations</td>
        <td>
            <div>Calculations to register for this form</div>
            <table>
                <th>name</th>
                <th>description</th>
                <tr>
                    <td>Form Id</td>
                    <td>The Form Id that this calculation belongs to. It is possible to execute calculations done on a field of a different form and save them when the current form is submitted. This should be tthe same as the "ID" field registered into calc.js.</td>
                </tr>
                <tr>
                    <td>Calculation Order</td>
                    <td>The order to execute this calculation in. The order does not have to be unique, but it is recommended to have them unique. After saving the calculations, they will be sorted based on this value.</td>
                </tr>
                <tr>
                    <td>Field Name</td>
                    <td>The name of the field that this calculation is for</td>
                </tr>
                <tr>
                    <td>Repeating</td>
                    <td>Whether or not the field is a repeating (array) field or not</td>
                </tr>
                <tr>
                    <td>Formula</td>
                    <td>The formula to use for calculations (writtent in JavaScript)<br/><em>As a general rule, only set the value for the corresponding field's name (setting intermediate variables that you define here are ok)</em></td>
                </tr>
                <tr>
                    <td>Always Execute</td>
                    <td>Whether or not the calculation for this field should always be executed (useful for rounding)</td>
                </tr>
                <tr>
                    <td>Set a label</td>
                    <td>Whether or not the framework should update a label (some read-only data without a means to provide or update input from the user)</td>
                </tr>
                <tr>
                    <td>Active</td>
                    <td>Whether or not this field's calculation in particular is active or not</td>
                </tr>
            </table>
        </td>
    </tr>
</table>

**How to reference form fields in calculations**

In general for fields within the current form
${field_name}
<pre><code>//Example with SampleForm1
alert(field1);</code></pre>
In general for fields from dependent forms
${form_id}.${field_name}
<pre><code>//Example with referencing SampleForm2's fields in SampleForm1
alert(SampleForm2.field1);</code></pre>

The way to access field data per data type
<table border="1">
    <th>type</th>
    <th>current form</th>
    <th>dependent form</th>
    <tr>
        <td>number</td>
        <td>${field_name}</td>
        <td>${form_id}.${field_name}</td>
    </tr>
    <tr>
        <td>decimal</td>
        <td>${field_name}</td>
        <td>${form_id}.${field_name}</td>
    </tr>
    <tr>
        <td>string</td>
        <td>${field_name}</td>
        <td>${form_id}.${field_name}</td>
    </tr>
    <tr>
        <td>boolean</td>
        <td>${field_name}</td>
        <td>${form_id}.${field_name}</td>
    </tr>
    <tr>
        <td>zipcode</td>
        <td>${field_name}_zip1<br/>${field_name}_zip2</td>
        <td>${form_id}.${field_name}.zip1<br/>${form_id}.${field_name}.zip2</td>
    </tr>
    <tr>
        <td>jdate</td>
        <td>${field_name}_era<br/>${field_name}_year<br/>${field_name}_month<br/>${field_name}_day</td>
        <td>${form_id}.${field_name}.era<br/>${form_id}.${field_name}.year<br/>${form_id}.${field_name}.month<br/>${form_id}.${field_name}.day</td>
    </tr>
    <tr>
        <td>yymm jdate</td>
        <td>${field_name}_era<br/>${field_name}_yy<br/>${field_name}_mm</td>
        <td>${form_id}.${field_name}.era<br/>${form_id}.${field_name}.yy<br/>${form_id}.${field_name}.mm</td>
    </tr>
    <tr>
        <td>yy jdate</td>
        <td>${field_name}_era<br/>${field_name}_yy</td>
        <td>${form_id}.${field_name}.era<br/>${form_id}.${field_name}.yy</td>
    </tr>
    <tr>
        <td>value label</td>
        <td>${field_name}_value<br/>${field_name}_label</td>
        <td>${form_id}.${field_name}.value<br/>${form_id}.${field_name}.label</td>
    </tr>
    <tr>
        <td>id ref</td>
        <td>${field_name}</td>
        <td>${form_id}.${field_name}</td>
    </tr>
    <tr>
        <td>tel num</td>
        <td>${field_name}_tel1<br/>${field_name}_tel2<br/>${field_name}_tel3</td>
        <td>${form_id}.${field_name}.tel1<br/>${form_id}.${field_name}.tel2<br/>${form_id}.${field_name}.tel3</td>
    </tr>
    <tr>
        <td>mmdd jdate</td>
        <td>${field_name}_mm<br/>${field_name}_dd</td>
        <td>${form_id}.${field_name}.mm<br/>${form_id}.${field_name}.dd</td>
    </tr>
    <tr>
        <td>yymmdd jdate</td>
        <td>${field_name}_era<br/>${field_name}_yy<br/>${field_name}_mm<br/>${field_name}_dd</td>
        <td>${form_id}.${field_name}.era<br/>${form_id}.${field_name}.yy<br/>${form_id}.${field_name}.mm<br/>${form_id}.${field_name}.dd</td>
    </tr>
</table>

**Provided calculation utility/helper functions**

*abs(x)*

Finds the absolute value of the given value.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The absolute value of x. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.abs(-10));  //10
</code></pre>

*abs_acc(x)*

Finds the absolute value of the given value (works more accurately for floating point numbers).

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The absolute value of x. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.abs_acc(-10.34323));  //10.34323
</code></pre>

*acos(x)*

Returns the arccosine of x (radians).

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The arccosine of x (radians). If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.acos(0.5));  //1.0471975511965976
</code></pre>

*add(x, y, z, ... or [x, y, z, ...])*

Adds all the given input.

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The sum of the parameters. Blank fields will be ignored in the sum. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.add(1, 2, 3));  //6
alert(calc.add([4, 5, 6, undefined, "", null]));  //15
</code></pre>

*add_acc(x, y, z, ... or [x, y, z, ...])*

Adds all the given input (works more accurately for floating point numbers).

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers (Note: to maintain accuracy, it is better to include the floating point numbers as strings)</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The sum of the parameters. Blank fields will be ignored in the sum. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.add_acc(1, "100.1", 3));  //104.1
alert(calc.add_acc([4, "500.3", 6, undefined, "", null]));  //510.3
</code></pre>

*asin(x)*

Returns the arcsine of x (radians).

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The arcsine of x (radians). If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.asin(0.5));  //0.5235987755982988
</code></pre>

*atan(x)*

Returns the arctangent of x as a numeric value between -PI/2 and PI/2 radians.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The arctangent of x as a numeric value between -PI/2 and PI/2 radians. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.atan(2));  //1.1071487177940906
</code></pre>

*atan2(y, x)*

Returns the arctangent of the quotient of its arguments.

<table>
    <tr>
        <td>Input</td>
        <td>y, x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The arctangent of the quotient of its arguments. If x and/or y is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.atan2(8, 4));  //1.1071487177940904
</code></pre>

*ceil(x)*

Returns x, rounded upwards to the nearest integer.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x rounded upwards to the nearest integer. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.ceil(1.2));  //2
</code></pre>

*cos(x)*

Returns the cosine of x.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number in radians</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The cosine of x. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.cos(3));  //-0.9899924966004454
</code></pre>

*counts(list, condition)*

<table>
    <tr>
        <td>Input</td>
        <td>list: An array of elements<br/>condition: A string representation of a condition. The value of the current element must be referenced by "val".</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The number of elements in the list that match the given condition. Empty elements in the list will not be counted.</td>
    </tr>
</table>
<pre><code>alert(calc.counts([-1,2,3,0, null, undefined, ""], "val > 0"));  //2
</code></pre>

*divide(x, y)*

Returns the result of division of x and y.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The result of division of x and y. If x and/or y is empty, it will return undefined. If y is 0, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.divide(1, 2));  //0.5
</code></pre>

*exp(x)*

Returns the value of E^x.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The the value of E^x. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.exp(1));  //2.718281828459045
</code></pre>

*floor(x)*

Returns x, rounded downwards to the nearest integer.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x rounded downwards to the nearest integer. If x is empty, it will return undefined.</td>
    </tr>
</table>
<pre><code>alert(calc.floor(123.433));  //123
</code></pre>

*get_repeating_field_array(fieldName)*

Gets the repeating fields' values as an array from the UI components.

<table>
    <tr>
        <td>Input</td>
        <td>fieldName: String name of the repeating field</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>Array of values taken from the UI components</td>
    </tr>
</table>
<pre><code>var F_01_val_array = calc.get_repeating_field_array("F_01");
for (var i=0; i<F_01_val_array.length; i++) {
    alert(i + ": " + F_01_val_array[i]);
}
</code></pre>

*is_empty(x)*

Determines whether or not x is empty or not. undefined, null, "", and a string with only whitespace is considered to be empty.

<table>
    <tr>
        <td>Input</td>
        <td>x: Any value</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>true if empty; false otherwise.</td>
    </tr>
</table>
<pre><code>alert(calc.is_empty(""));  //true
</code></pre>

*limit(lower_bound, x, upper_bound)*

Gets the value of x within the specified lower and upper bounds.

<table>
    <tr>
        <td>Input</td>
        <td>lower_bound: Number or "*" to represent negative infinity.<br/>x: Number<br/>upper_bound: Number or "*" to represent positive infinity.</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The value of x within the specified lower and upper bounds. undefined is returned if any parameter is empty.</td>
    </tr>
</table>
<pre><code>alert(calc.limit("*", 100, 50));  //50
</code></pre>

*log(x)*

Returns the natural logarithm (base E) of x.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The natural logarithm (base E) of x.</td>
    </tr>
</table>
<pre><code>alert(calc.log(100));  //4.605170185988092
</code></pre>

*max(x, y, z, ... or [x, y, z, ...])*

Returns the maximum value of the parameters or array. Empty values are considered as the lowest possible value.

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The max of the parameters. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.max(1, 2, 3));  //3
alert(calc.max([4, 5, 6, undefined, "", null]));  //6
</code></pre>

*min(x, y, z, ... or [x, y, z, ...])*

Returns the minimum value of the parameters or array. Empty values are considered as the lowest possible value.

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The min of the parameters. If even a single parameter is blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.min(1, 2, 3));  //1
alert(calc.min([4, 5, 6, undefined, "", null]));  //undefined
</code></pre>

*mod(x, y)*

Returns the modulus of x and y.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The modulus of x and y. If even a single parameter is blank, undefined will be returned. If y is 0, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.mod(9, 5));  //4
</code></pre>

*multiply(x, y, z, ... or [x, y, z, ...])*

Returns the product of the parameters.

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The product of the parameters. If any field is blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.multiply(1, 2, 3));  //6
alert(calc.multiply([4, 5, 6, undefined, "", null]));  //undefined
</code></pre>

*multiply_acc(x, y, z, ... or [x, y, z, ...])*

Returns the product of the parameters (works more accurately for floating point numbers).

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers (Note: to maintain accuracy, it is better to include the floating point numbers as strings)</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The product of the parameters. If any field is blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.multiply_acc(1, "1.1", 3));  //3.3
alert(calc.multiply_acc([4, "5.5", 6, undefined, "", null]));  //undefined
</code></pre>

*pow(x, y)*

Returns x to the power of y.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x to the power of y. If even a single parameter is blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.pow(2, 3));  //8
</code></pre>

*quotient(x, y)*

Returns the quotient of x/y.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The quotient of x/y. If even a single parameter is blank, undefined will be returned. If y is 0, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.quotient(6, 5));  //1
</code></pre>

*remove_commas(value)*

Removes commas from a string.

<table>
    <tr>
        <td>Input</td>
        <td>value: String</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>String without commas.</td>
    </tr>
</table>
<pre><code>alert(calc.remove_commas("123,456"));  //123456
</code></pre>

*round(x, y)*

Rounds x to the y-th place.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x rounded to the yth-place. If either x or y is blank, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.round(153.473, 1));  //153.5
</code></pre>

*round_down(x, y)*

Rounds x down to the y-th place.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x rounded down to the yth-place. If either x or y is blank, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.round_down(153.473, 2));  //153.47
</code></pre>

*round_up(x, y)*

Rounds x up to the y-th place.

<table>
    <tr>
        <td>Input</td>
        <td>x, y: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>x rounded up to the yth-place. If either x or y is blank, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.round_up(153.473, -1));  //160
</code></pre>

* sin(x)

Returns the sine of x.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number in radians</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The sine of x. If x is blank, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.sin(3));  //0.1411200080598672
</code></pre>

* sqrt(x)

Returns the square root of x.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The square root of x. If x is blank, undefined is returned.</td>
    </tr>
</table>
<pre><code>alert(calc.sqrt(4));  //2
</code></pre>

*subtract(x, y, z, ... or [x, y, z, ...])*

Subtracts all the given input.

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The consecutive subtraction of the parameters. Blank fields will be ignored in the result. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.subtract(1, 2, 3));  //-4
alert(calc.subtract([undefined, 4, 5, -6, undefined, "", null]));  //5
</code></pre>

*subtract_acc(x, y, z, ... or [x, y, z, ...])*

Subtracts all the given input (works more accurately for floating point numbers).

<table>
    <tr>
        <td>Input</td>
        <td>x, y, z, ... OR [x, y, z, ...]: List of numbers or array of numbers (Note: to maintain accuracy, it is better to include the floating point numbers as strings)</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The consecutive subtraction of the parameters. Blank fields will be ignored in the result. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.subtract_acc("-10.5", 2, 3));  //-15.5
alert(calc.subtract_acc([undefined, 4, 5, -6, undefined, "", null]));  //5
</code></pre>

*sum_if(condition, elements)*

Sums the elements if the condition is true.

<table>
    <tr>
        <td>Input</td>
        <td>condition: Boolean<br/>elements: Array of Numbers</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The sum of the elements if the condition is true. Blank fields will be ignored in the result. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>var a = 1;
alert(calc.sum_if(a > 0, [1, 2, 3]));  //6
</code></pre>

*sum_if_acc(condition, elements)*

Sums the elements if the condition is true (works more accurately for floating point numbers).

<table>
    <tr>
        <td>Input</td>
        <td>condition: Boolean<br/>elements: Array of Numbers (Note: to maintain accuracy, it is better to include the floating point numbers as strings)</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The sum of the elements if the condition is true. Blank fields will be ignored in the result. If all fields are blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>var a = 1;
alert(calc.sum_if_acc(a > 0, [1, 2, "3.3"]));  //6.3
</code></pre>

*tan(x)*

Returns the tangent of an angle.

<table>
    <tr>
        <td>Input</td>
        <td>x: Number</td>
    </tr>
    <tr>
        <td>Output</td>
        <td>The tangent of x. If x is blank, undefined will be returned.</td>
    </tr>
</table>
<pre><code>alert(calc.tan(90));  //-1.995200412208242
</code></pre>
