# Microtope

Hey there! This is my try on a multi-language microservice architecture!
Hope you enjoy your stay. Feel free to contribute to any language and project fit for you.

I also would love someone who just runs the project on his docker and opens issues wherever he finds some.
The bad words you'll hear are just a way to express my happyness.

## Architecture

The Application currently is quite stringend:

1. A Java Application creates messages and puts it into an ActiveMQ-Artemis
2. An ActiveMQ Image is hosted
3. Another Java Application Reads messages from an ActiveMQ-Artemis and inserts them into an Mariadb
4. The Mariadb holds data and checks for (most of the) integrity
5. *Wesir* iterates of the Logins and Logouts in MariaDB and deletes anomalies, and verifies good entries
6. An API is able to read the mariadb on demand and extract data as JSON-Documents
7. An UI is able to display and interact with the data granted from the API

![Architecture](./meta/documentation/microtope_overview.png)

## How to run

To run the Application, look into [the meta folder](./meta). This folder will contain any instructions required to run it on docker-compose or kubernetes.

## Licence

I'm using the [MIT Licence](./LICENCE). Therefore, feel free to use anything you like here.

You are not forced to do anything for me or contribute your changes.

However, please pay respect the licences of the used dependencies!

## Contributing

Feel free to add any issue as you wish or fork the repository. I'm always happy about any feedback I can get.

If you want to contribute Code, please read the [Contribution-Guidelines](./CONTRIBUTING.md).

Additionally, please stick to the provided templates:

- [For Commits](.github/COMMIT_TEMPLATE/commit.template)
- [For PRs](.github/PULL_REQUEST_TEMPLATE/pull_request_template.md)

You can ensure your policies by using a local git configuration via:

`$> git config --local commit.template=./.github/COMMIT_TEMPLATE/commitmessage.txt`

(Perform this on the project root!)

Additonally you can squash and edit your commit-message to fit the template.

For later versions, the use of these templates will be enforced. I know, this is hard, but so is life.

If you alter the code and want to notice me, but don't want to go the long PR-Way, please open a feedback-issue with a link to your fork.

## Preview Image

The Preview Image was taken from [Iconfinder](https://www.iconfinder.com/icons/3285297/andromeda_astronomy_cosmos_galaxy_space_spiral_universe_icon) in respect to the [Creative-Commons 3 Licence](https://creativecommons.org/licenses/by/3.0/). If you're the artist and ever encounter this repository, please reach out to me and maybe we can drink a coffee!
