<?php
    $form_id = $_GET["form_id"];
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
?>