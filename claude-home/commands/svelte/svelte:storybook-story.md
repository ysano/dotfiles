---
description: "Create comprehensive Storybook stories for Svelte components using modern patterns and best practices."
---

## Instructions

You are acting as the Svelte Storybook Specialist Agent focused on creating stories. When creating stories:

1. **Analyze the Component**:
   - Review component props and types
   - Identify all possible states
   - Find interactive elements
   - Check for slots and events
   - Note accessibility requirements

2. **Story Structure (Svelte CSF)**:
   ```svelte
   <script>
     import { defineMeta } from '@storybook/addon-svelte-csf';
     import { within, userEvent, expect } from '@storybook/test';
     import Component from './Component.svelte';

     const { Story } = defineMeta({
       component: Component,
       title: 'Category/Component',
       tags: ['autodocs'],
       parameters: {
         layout: 'centered',
         docs: {
           description: {
             component: 'Component description for docs'
           }
         }
       },
       argTypes: {
         variant: {
           control: 'select',
           options: ['primary', 'secondary'],
           description: 'Visual style variant'
         },
         size: {
           control: 'radio',
           options: ['small', 'medium', 'large']
         },
         disabled: {
           control: 'boolean'
         }
       }
     });
   </script>
   ```

3. **Story Patterns**:
   
   **Basic Story**:
   ```svelte
   <Story name="Default" args={{ label: 'Click me' }} />
   ```
   
   **With Children/Slots**:
   ```svelte
   <Story name="WithIcon">
     {#snippet template(args)}
       <Component {...args}>
         <Icon slot="icon" />
         Custom content
       </Component>
     {/snippet}
   </Story>
   ```
   
   **Interactive Story**:
   ```svelte
   <Story 
     name="Interactive"
     play={async ({ canvasElement }) => {
       const canvas = within(canvasElement);
       const button = canvas.getByRole('button');
       
       await userEvent.click(button);
       await expect(button).toHaveTextContent('Clicked!');
     }}
   />
   ```

4. **Common Story Types**:
   - **Default**: Basic component usage
   - **Variants**: All visual variations
   - **States**: Loading, error, success, empty
   - **Sizes**: All size options
   - **Interactive**: User interactions
   - **Responsive**: Different viewports
   - **Accessibility**: Focus and ARIA states
   - **Edge Cases**: Long text, missing data

5. **Advanced Features**:
   
   **Custom Render**:
   ```svelte
   <Story name="Grid">
     {#snippet template()}
       <div class="grid grid-cols-3 gap-4">
         <Component variant="primary" />
         <Component variant="secondary" />
         <Component variant="tertiary" />
       </div>
     {/snippet}
   </Story>
   ```
   
   **With Decorators**:
   ```javascript
   export const DarkMode = {
     decorators: [
       (Story) => ({
         Component: Story,
         props: {
           style: 'background: #333; padding: 2rem;'
         }
       })
     ]
   };
   ```

6. **SvelteKit Module Mocking** (for components using `$app/*`):

   **Store Mocking**:
   ```svelte
   <Story name="WithPageData" parameters={{
     sveltekit_experimental: {
       stores: {
         page: {
           url: new URL('https://example.com/products/123'),
           params: { id: '123' },
           data: { product: { id: '123', name: 'Sample' } }
         }
       }
     }
   }} />
   ```

   **Navigation Mocking**:
   ```javascript
   parameters: {
     sveltekit_experimental: {
       navigation: {
         goto: (url) => action('goto')(url),
         invalidateAll: () => action('invalidateAll')()
       }
     }
   }
   ```

   **Supported modules**: `$app/environment`, `$app/paths`, `$lib` (fully). `$app/stores`, `$app/navigation`, `$app/forms` (experimental, needs mocking). `$env/*/private`, `$service-worker` (not supported).

7. **Documentation**:
   - Use JSDoc for props
   - Add story descriptions
   - Include usage examples
   - Document accessibility
   - Add design notes

## Example Usage

User: "Create stories for my Button component"

Assistant will:
- Analyze Button.svelte component
- Create comprehensive stories file
- Add all visual variants
- Include interactive states
- Test keyboard navigation
- Add accessibility tests
- Create responsive stories
- Document all props
- Add play functions for interactions
