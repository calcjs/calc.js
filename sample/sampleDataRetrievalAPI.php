<?php
    $arr = array ( 'data' => array (
    	array ('name' => "field1", 'value' => 111, 'type' => 0),
    	array ('name' => "field2", 'value' => 222, 'type' => 0),
    	array ('name' => "field3", 'value' => 333, 'type' => 0)
    ), 'correctedFields' => array('field1'));
    echo json_encode($arr);
?>