
import React, { useState } from 'react';
import axios from 'axios';
import buttonJson from './multispecies_model_options.json';

const App = () => {
  const [formFields, setFormFields] = useState({});
  const [selectedModels, setSelectedModels] = useState([]);
  const [modelResults, setModelResults] = useState(null);
  const [buttons, setButtons] = useState([]);

  // Function to call /getInputs and fetch input fields for models
  const handleModelSelect = async (model) => {
    let modelName = '';

    if (model === 'Model1') modelName = 'OrthoFinder';
    else if (model === 'Model2') modelName = 'CDROM';
    else if (model === 'Model3') modelName = 'Public Datasets';

    // Toggle selection of model
    const updatedModels = selectedModels.includes(modelName)
      ? selectedModels.filter((m) => m !== modelName)
      : [...selectedModels, modelName];
    setSelectedModels(updatedModels);

    if (!updatedModels.includes(modelName)) {
      // Remove buttons and inputs if the model is deselected
      setFormFields((prev) => {
        const { [modelName]: _, ...remainingFields } = prev;
        return remainingFields;
      });
      setButtons([]);  // Clear the dynamically generated buttons
      return;
    }

    try {
      const response = await axios.post('http://localhost:8000/getInputs', {
        selected_models: updatedModels.join(', '),
        parameters: {},
      });

      console.log('API Response for getInputs:', response.data);

      // Combine required and additional buttons into one list
      const combinedButtonList = [
        ...response.data.required_button_list.flat(),
        ...response.data.additional_button_list.flat(),
      ];

      // Map button names to their metadata from the JSON file
      const buttonMetadata = combinedButtonList.map((buttonName) => {
        const metadata = buttonJson[buttonName]; // Assuming `buttonJson` is imported
        return { name: buttonName, ...metadata };
      });

      setButtons(buttonMetadata); // Set buttons to be rendered

      // Initialize form fields with the default values from the API response
      const newFormFields = updatedModels.reduce((acc, model) => {
        acc[model] = combinedButtonList.map((buttonName) => {
          const metadata = buttonJson[buttonName];
          return {
            name: buttonName,
            label: metadata.label,
            type: metadata.type,
            value: metadata.default ?? '', // Default value from JSON
            choices: metadata.choices ?? [],
            multiple: metadata.multiple ?? false,
          };
        });
        return acc;
      }, {});

      setFormFields(newFormFields);
    } catch (error) {
      console.error('Error fetching input fields:', error);
    }
  };

  // Function to handle form input change
  const handleInputChange = (e, modelName, index) => {
    const { name, type, value, checked } = e.target;
    const fieldValue = type === 'checkbox' ? checked : value;

    setFormFields((prevFields) => {
      const updatedFields = prevFields[modelName].map((field, idx) =>
        idx === index ? { ...field, value: fieldValue } : field
      );
      return { ...prevFields, [modelName]: updatedFields };
    });
  };

  // Function to submit the form and call /runModel
  const handleSubmit = async (e) => {
    e.preventDefault();

    const modelsWithParams = selectedModels.map((modelName) => {
      const parameters = formFields[modelName]?.reduce((acc, field) => {
        acc[field.name] = field.value;
        return acc;
      }, {});
      console.log('Model parameters:', parameters);  // Log the parameters to verify

      return { model: modelName, parameters };
    });

    console.log('Parameters to submit:', modelsWithParams);

    try {
      const response = await axios.post('http://localhost:8000/runModel', {
        selected_models: selectedModels.join(', '),
        parameters: modelsWithParams,
      });
      console.log('API Response for runModel:', response.data);
      setModelResults(response.data);
    } catch (error) {
      console.error('Error running models:', error);
    }
  };

  // Function to render dynamic buttons based on the buttonJson metadata
  const renderDynamicButton = (button, modelName, index) => {
    const { type, label, name, choices, default: defaultValue, multiple } = button;

    switch (type) {
      case 'text':
        return (
          <div key={name}>
            <label>{label}</label>
            <input
              type="text"
              name={name}
              value={formFields[modelName]?.[index]?.value ?? defaultValue}
              onChange={(e) => handleInputChange(e, modelName, index)}
            />
          </div>
        );
      case 'select':
        return (
          <div key={name}>
            <label>{label}</label>
            <select
              name={name}
              value={formFields[modelName]?.[index]?.value ?? defaultValue}
              onChange={(e) => handleInputChange(e, modelName, index)}
            >
              {choices.map((choice, idx) => (
                <option key={idx} value={choice}>
                  {choice}
                </option>
              ))}
            </select>
          </div>
        );
      case 'boolean':
        return (
          <div key={name}>
            <label>{label}</label>
            <input
              type="checkbox"
              name={name}
              checked={formFields[modelName]?.[index]?.value ?? defaultValue}
              onChange={(e) => handleInputChange(e, modelName, index)}
            />
          </div>
        );
      case 'number':
        return (
          <div key={name}>
            <label>{label}</label>
            <input
              type="number"
              name={name}
              value={formFields[modelName]?.[index]?.value ?? defaultValue}
              onChange={(e) => handleInputChange(e, modelName, index)}
            />
          </div>
        );
      case 'directory':
        return (
          <div key={name}>
            <label>{label}</label>
            <input
              type="file"
              name={name}
              webkitdirectory="true"
              onChange={(e) => handleInputChange(e, modelName, index)}
            />
          </div>
        );
      case 'file':
        return (
          <div key={name}>
            <label>{label}</label>
            <input
              type="file"
              name={name}
              multiple={multiple}
              onChange={(e) => handleInputChange(e, modelName, index)}
            />
          </div>
        );
      default:
        return null;
    }
  };

  return (
    <div>
      <h1>Select Models</h1>
      <div>
        <button
          onClick={() => handleModelSelect('Model1')}
          style={{ backgroundColor: selectedModels.includes('OrthoFinder') ? 'lightblue' : 'white' }}
        >
          OrthoFinder
        </button>
        <button
          onClick={() => handleModelSelect('Model2')}
          style={{ backgroundColor: selectedModels.includes('CDROM') ? 'lightblue' : 'white' }}
        >
          CDROM
        </button>
        <button
          onClick={() => handleModelSelect('Model3')}
          style={{ backgroundColor: selectedModels.includes('Public Datasets') ? 'lightblue' : 'white' }}
        >
          Public Datasets
        </button>
      </div>

      <form onSubmit={handleSubmit}>
        <h2>Input Parameters</h2>
        {selectedModels.map((modelName) => (
          <div key={modelName}>
            <h3>{modelName}</h3>
            {formFields[modelName]?.map((field, index) =>
              renderDynamicButton(field, modelName, index)
            )}
          </div>
        ))}

        {/* Dynamically render buttons */}
        <h2>Dynamic Buttons</h2>
        {buttons.length > 0 && (
          <div>
            {buttons.map((button, index) => renderDynamicButton(button, 'modelName', index))}
          </div>
        )}

        <button type="submit">Run Models</button>
      </form>

      {modelResults && (
        <div>
          <h2>Results</h2>
          <pre>{JSON.stringify(modelResults, null, 2)}</pre>
        </div>
      )}
    </div>
  );
};

export default App;

