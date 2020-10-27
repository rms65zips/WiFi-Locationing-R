# WiFi-Locationing

### Project Objectives
* Investigate feasibility of using “WiFi Fingerprinting” to determine a person’s location in indoor spaces.
* Use data provided from multiple WiFi hotspots within each building to determine location.
* Evaluate multiple machine learning models to see what produces the best results.
* Make recommendations to clients based on model results.
* Improve model accuracy, if necessary, so it can be incorporated into a smartphone app for indoor locationing.

### Data Source
Data was taken from http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc

### Training Data Features
* 001 - 520: RSSI levels
    Values contain raw intensity levels of the detected WAP from a single WiFi scan.  RSSI levels are negative values measured in dBm, where -100dBm is a very weak signal and 0dBm is a strong signal.
    Undetected WAPS are labeled +100.
    
* 521-522:  Real world coordinates
    Longitude and Latitude
    
* 523:  Floor
    Floor number in each building
    
* 524:  Building ID
    Corresponds to building WAP signal was captured
    
* 525:  Space ID
    Space (offices, labs, classroom, etc.) WAP signal was captured
    
* 526:  Relative Position
    Inside or outside Space ID
    
* 527:  User ID
    Represents different users
    
* 528:  Phone ID
    Represents device used
    
* 529:  Timestamp
    Time (in Unix format) WAP signal was captured
