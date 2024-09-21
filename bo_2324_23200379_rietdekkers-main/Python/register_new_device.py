import os, sys
import psycopg2
import csv

# Definition of database class
class DbConnection:

    # Constructor
    def __init__(self):

        # Set constante
        self.SRID = 4326

        # Open database connection
        try:
            path = os.path.dirname(os.path.abspath(sys.argv[0]))
            db_file = open(path + '/db.txt','r')
            delimiter = ':'
            for row in db_file:
                db_host = row.split(delimiter)[0]
                db_port = row.split(delimiter)[1]
                db_name = row.split(delimiter)[2]
                db_user = row.split(delimiter)[3]
                db_password = row.split(delimiter)[4]
            db_file.close()
            db_connect = "dbname='" + str(db_name) + "' " 
            db_connect = db_connect + "user='" + str(db_user) + "' " 
            db_connect = db_connect + "host='" + str(db_host) + "' " 
            db_connect = db_connect + "port='" + str(db_port) + "' " 
            db_connect = db_connect + "password='" + str(db_password) + "'"
            self.db_connect = db_connect
        except :
            raise

    # Open connection
    def open_connection(self):
        try:
            self.conn = psycopg2.connect(self.db_connect)
            self.cur = self.conn.cursor()
            return True
        except :
            return False

    # Toevoegen fieldlab
    def add_fieldlab(self, input_params):
        try:

            # Build insert statement
            sql_insert = 'insert into public.fieldlab'
            sql_insert = sql_insert + '(fieldlab_name, fieldlab_description, location)'
            sql_insert = sql_insert + ' values (%s,%s,ST_SetSRID(ST_MakePoint(%s,%s),%s))'

            # Values to insert
            values = []
            values.append(input_params['fieldlab_name'])
            values.append(input_params['fieldlab_description'])
            values.append(input_params['lon'])
            values.append(input_params['lat'])
            values.append(self.SRID)

            # Execute query
            self.cur.execute(sql_insert,values) 
 
        except Exception as e:
            print("Exception occurred adding fieldlabs")
            print(e)
            self.rollback()
            raise 

    # Toevoegen van een device met sensoren
    def add_device(self, input_parameters):
        try:

            # Insert statement to add device 		
            sql_insert = 'INSERT INTO public.device('
            sql_insert = sql_insert + ' end_dev_eui, model_id, installation_description)'
            sql_insert = sql_insert + ' VALUES (%s, %s, %s)'

            # Make list of device parameters
            device_attributes = []
            device_attributes.append(input_parameters['end_dev_eui'])
            device_attributes.append(input_parameters['model_id'])
            device_attributes.append(input_parameters['installation_description'])

            # Insert device
            print("Insert device " + str(input_parameters['end_dev_eui']))
            self.cur.execute(sql_insert, device_attributes)
                
        except Exception as e:
            print("Exception occurred adding device")
            print(e)
            self.rollback()
            raise 

    # Toevoegen fieldlab
    def add_tag(self, input_params):
        try:

            # Build insert statement
            sql_insert = 'INSERT INTO public.tag(tag) VALUES(%s)'

            # Values to insert
            values = []
            values.append(input_params['tag'])

            # Execute query
            self.cur.execute(sql_insert,values) 
 
        except Exception as e:
            print("Exception occurred adding tag")
            print(e)
            self.rollback()
            raise 

    # Toevoegen device aan fieldlab
    def add_device_to_fieldlab(self, input_params):
        try:

            # Get parameters
            fieldlab_name = input_params['fieldlab_name'] 
            end_dev_eui = input_params['end_dev_eui']
            tag = input_params['tag'] 
            start_date = input_params['start_date'] 

            # Get ID fieldlab
            sql_stmt = 'select fieldlab_id from fieldlab where fieldlab_name = %s' 
            self.cur.execute(sql_stmt,(fieldlab_name,)) 
            fieldlab_id = self.cur.fetchone()[0]   

            # Make current installation historical
            sql_update = 'update fieldlab_device set end_date = %s where end_dev_eui = %s'
            self.cur.execute(sql_update,(start_date, end_dev_eui)) 

            # Build insert statement
            sql_insert = 'INSERT INTO fieldlab_device'
            sql_insert = sql_insert + '(fieldlab_id, end_dev_eui, tag, start_date)' 
            sql_insert = sql_insert + ' values(%s, %s, %s, %s)'

            # Values to insert
            values = []
            values.append(fieldlab_id)
            values.append(end_dev_eui)
            values.append(tag)
            values.append(start_date)

            # Execute query
            self.cur.execute(sql_insert,values)  

        except Exception as e:
            print("Exception occurred adding tag")
            print(e)
            self.rollback()
            raise        

    # Add model
    def add_model(self, input_params):
        try:

            # Build insert statement
            sql_insert = 'INSERT INTO public.model(model_id, model_url) VALUES(%s,%s)'

            # Values to insert
            values = []
            values.append(input_params['model_id'])
            values.append(input_params['model_url'])

            # Execute query
            self.cur.execute(sql_insert,values) 

        except Exception as e:
            print("Model already inserted")
            self.rollback()
        
    # Add parameter
    def add_parameter(self, input_params):
        try:

            # Build insert statement
            sql_insert = 'INSERT INTO parameter'
            sql_insert = sql_insert + '(parameter_name, parameter_description, parameter_unit, model_id)' 
            sql_insert = sql_insert + ' values(%s, %s, %s, %s)'

            # Values to insert
            values = []
            values.append( input_params['parameter_name'] )
            values.append( input_params['parameter_description'] )
            values.append( input_params['parameter_unit'] )
            values.append( input_params['model_id'] )

            # Execute query
            self.cur.execute(sql_insert,values) 

        except Exception as e:
            print("Exception occurred adding tag")
            print(e)
            self.rollback()
            raise 
        
    # Commit
    def commit(self):
        self.conn.commit()
        
    # Close
    def close(self):
        try:
            self.conn.close()
        except:
            print("Exception occurred closing database")
            raise
        
    # Rollback
    def rollback(self):
        self.conn.rollback()
        
    # Commit and close
    def commit_and_close(self):
        try:
            self.conn.commit()
            self.conn.close()
        except Exception as e:
            print("Exception occurred closing database " + str(e))
            raise

# Start script
print('Start script')

# What do you want to do?
ADD_FIELDLAB = False
ADD_DEVICE = False
ADD_TAG = False
ADD_DEVICE_TO_FIELDLAB = True

# Open DB connection
print('Open DB connection')
sensor_db = DbConnection()
sensor_db.open_connection() 

# Add fieldlab'
if ADD_FIELDLAB:
    print('Add fieldlab')
    fieldlab_params = {}
    fieldlab_params['fieldlab_name'] = 'Mark Fieldlab'
    fieldlab_params['fieldlab_description'] = 'Op dak van HAS' 
    fieldlab_params['lon'] = 3.1 
    fieldlab_params['lat'] = 53.2 
    sensor_db.add_fieldlab(fieldlab_params)

# Add device
if ADD_DEVICE:
    print('Add device')
    device_params = {}
    device_params['end_dev_eui'] = 'Temp device'
    device_params['installation_description'] = 'Op dak' 
    device_params['model_id'] = 'LSE01' #LSE01/sensecaps2120-8-in-1/lsn50v2-s31/MCF-LW12TERPM
    sensor_db.add_device(device_params)

# Add device
if ADD_TAG:
    print('Add tag')
    tag_params = {}
    tag_params['tag'] = 'groen dak'
    sensor_db.add_tag(tag_params)

# Add device to fieldlab
if ADD_DEVICE_TO_FIELDLAB:
    print('Add device to fieldlab')
    device_to_fieldlab_params = {}
    device_to_fieldlab_params['fieldlab_name'] = 'Mark Fieldlab'
    device_to_fieldlab_params['end_dev_eui'] = 'Temp device'
    device_to_fieldlab_params['tag'] = 'groen dak'
    device_to_fieldlab_params['start_date'] = '2024-03-05'
    sensor_db.add_device_to_fieldlab(device_to_fieldlab_params)

# Commit
print('Commit')
sensor_db.commit_and_close()

# Start script
print('End script')
